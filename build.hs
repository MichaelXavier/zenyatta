#!/usr/bin/env stack
{- stack --resolver lts-8.12 --install-ghc
    runghc
    --package shake
-}
{-# LANGUAGE FlexibleContexts #-}

-------------------------------------------------------------------------------
import           Development.Shake
import           Development.Shake.FilePath
import Debug.Trace
-------------------------------------------------------------------------------


main :: IO ()
main = shakeArgs shakeOptions $ do
  want [
      "dist/index.html"
    , "dist/main.min.js"
    ]

  dirRule "bower_components" $ do
    let bowerPath = "node_modules/bower/bin/bower"
    needDirs ["node_modules"]
    need ["bower.json"]
    unit (cmd bowerPath ["update"])

  dirRule "node_modules" $ do
    need ["package.json"]
    unit (cmd "npm" ["install"])

  "dist/index.html" %> \out -> do
    copyFile' "static/index.html" out

  "dist/main.js" %> \out -> do
    needDirs ["node_modules"]
    need ["bower.json"]
    need =<< getDirectoryFiles "" ["**/*.purs"]
    cmd pulpPath [
        "browserify"
      , "--optimise"
      , "--to", out
      ]

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
    pulpPath = "node_modules/pulp/index.js"
    touch f = unit (cmd "touch" [f])
    needDirs dirs = need [ dir </> ".shake" | dir <- dirs]
    dirRule dir f = dir </> ".shake" %> \out -> do
      _ <- f
      touch out
