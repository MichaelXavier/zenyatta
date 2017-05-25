module Zenyatta.App
    ( view
    , foldp
    , initialState
    ) where


-------------------------------------------------------------------------------
import Data.Foreign as F
import Data.Lens as L
import DOM.Event.Event as DOMEE
import DOM.HTML as DOMH
import DOM.HTML.History as DOMHH
import DOM.HTML.Window as DOMHW
import Pux as P
import Pux.DOM.HTML as PH
import Zenyatta.Prelude
import Zenyatta.Types as T
import Zenyatta.Timer as Timer
import Text.Smolder.Markup (text)
-------------------------------------------------------------------------------


initialState :: T.State
initialState =
  { route: T.TimerR
  }


-------------------------------------------------------------------------------
view :: T.State -> PH.HTML T.Event
view s@{route: T.TimerR} = Timer.view s
view s@{route: T.LogsR} = text "TODO: logs"


-------------------------------------------------------------------------------
foldp :: T.Event -> T.State -> P.EffModel T.State T.Event (T.Effects ())
foldp T.NOOP s = P.noEffects s
foldp T.Tick s = P.noEffects s
foldp (T.PageView r) s = P.noEffects s { route = r}
foldp (T.Navigate r ev) st =
  P.onlyEffects st [ liftEff do
    maybe noop DOMEE.preventDefault ev
    h <- DOMHW.history =<< DOMH.window
    let url = L.review T.routeString r
    DOMHH.pushState (F.toForeign {}) (DOMHH.DocumentTitle "") (DOMHH.URL url) h
    pure (Just (T.PageView r))
  ]
