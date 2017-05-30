#!/usr/bin/env stack
{- stack --resolver lts-8.12 --install-ghc
    runghc
    --package shake
-}
{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------
import           Control.Monad
import           Development.Shake
import           Development.Shake.FilePath
-------------------------------------------------------------------------------


--TODO: vendor purescript
main :: IO ()
main = shakeArgs shakeOptions $ do
  want [
      "dist/index.html"
    , "dist/main.min.js"
    , "dist/main.css"
    , "dist/preact.min.js"
    , "dist/preact-compat.min.js"
    , "dist/proptypes.min.js"
    , "svgs"
    ]

  forM_ [
     "dist/preact.js"
   , "dist/preact-compat.js"
   , "dist/proptypes.js"
   ] $ \f -> f %> \out -> copyFile' ("static" </> takeFileName f) f

  phony "svgs" $ do
    getDirectoryFiles "static" ["*.svg"] >>= \svgs ->
      forM_ svgs $ \f -> copyFile' ("static" </> f) ("dist" </> f)

  dirRule "node_modules" $ do
    need ["package.json"]
    unit (cmd "npm" ["install"])

  "dist/index.html" %> \out -> do
    copyFile' "static/index.html" out

  --TODO: since we're not calling bundle, is this doing DCE?

  dirRule ".psc-package" $ do
    need ["psc-package.json", pscPackagePath]
    unit (cmd pscPackagePath ["update"])

  --FIXME: this is too eager
  phony "build" $ do
    needDirs [
        "node_modules"
      , ".psc-package"
      ]
    need =<< getDirectoryFiles "" ["src/**/*.purs"]
    unit (cmd pscPackagePath ["build"])

  "dist/main.js" %> \out -> do
    need [
        "rollup.config.js"
      , pscPackagePath
      , "build"
      ]
    unit (cmd "node_modules/rollup/bin/rollup" ["--config"])

  "dist/main.css" %> \out -> do
     need ["build"]
     Stdout output <- cmd
       "psc-bundle"
       [ "--main", "Zenyatta.CSS"
       , "output/Zenyatta.CSS/index.js"
       ]
     command_
       [ FileStdout out
       , Stdin output
       , Cwd "output/Zenyatta.CSS"
       ]
       "node"
       []

  "**/*.min.js" %> \out -> do
     let srcFile = out `replaceExtensions` "js"
     let uglifyJsPath = "node_modules/uglify-js/bin/uglifyjs"
     needDirs ["node_modules"]
     need [srcFile]
     command_
       [FileStdout out]
       uglifyJsPath
       [ "--compress", "warnings=false"
       , "--mangle"
       , "--"
       , srcFile
       ]
  where
    pscPackagePath = "vendor/psc-package/psc-package"
    touch f = unit (cmd "touch" [f])
    needDirs dirs = need [ dir </> ".shake" | dir <- dirs]
    dirRule dir f = dir </> ".shake" %> \out -> do
      _ <- f
      touch out
