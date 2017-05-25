module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Zenyatta.Prelude
import DOM as DOM
import DOM.HTML.Types as DOMHT
import Pux as P
import Pux.Renderer.React as React
import Signal as Signal
import Signal.Channel as Channel
import Signal.Time as ST
import Zenyatta.App as App
import Zenyatta.Types as T
import Control.Monad.Eff.Exception as EX
-------------------------------------------------------------------------------


-- This is ridiculous
main :: Eff ( channel :: Channel.CHANNEL
            , channel :: Channel.CHANNEL
            , dom :: DOM.DOM
            , exception :: EX.EXCEPTION
            , exception :: EX.EXCEPTION
            , history :: DOMHT.HISTORY) Unit
main = do
  app <- P.start
    { initialState: App.initialState
    , view: App.view
    , foldp: App.foldp
    , inputs: [Signal.sampleOn (ST.every ST.second) (Signal.constant T.Tick)]
    }

  React.renderToDOM "#app" app.markup app.input
