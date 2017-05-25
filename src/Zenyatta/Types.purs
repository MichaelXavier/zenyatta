module Zenyatta.Types
    ( State(..)
    , Event(..)
    , Route(..)
    , routeString
    , Effects
    ) where


-------------------------------------------------------------------------------
import Zenyatta.Prelude
import Control.Monad.Eff.Exception as EX
import DOM as DOM
import DOM.HTML.Types as DOMHT
import Data.Lens as L
import Pux.DOM.Events as PE
import Signal.Channel as Channel
-------------------------------------------------------------------------------


type State =
  { route :: Route
  }


-------------------------------------------------------------------------------
data Route = TimerR
           | LogsR


routeString :: L.Prism' String Route
routeString = L.prism' toS fromS
  where
    toS TimerR = "/timer"
    toS LogsR = "/logs"

    fromS "/timer" = Just TimerR
    fromS "/logs"  = Just LogsR
    fromS _        = Nothing


-------------------------------------------------------------------------------
data Event = NOOP
           | Tick -- ^ Event delivered every second
           | PageView Route
           | Navigate Route (Maybe PE.DOMEvent)


-------------------------------------------------------------------------------
type Effects eff =
  ( channel :: Channel.CHANNEL
  , exception :: EX.EXCEPTION
  , dom :: DOM.DOM
  , history :: DOMHT.HISTORY
  | eff
  )
