module Zenyatta.Types
    ( State(..)
    , TimerState(..)
    , Event(..)
    , TimerEvent(..)
    , Route(..)
    , routeString
    , Effects
    ) where


-------------------------------------------------------------------------------
import Zenyatta.Prelude
import Control.Monad.Eff.Exception as EX
import Data.Time.Duration as TD
import DOM as DOM
import DOM.HTML.Types as DOMHT
import Data.Lens as L
import Pux.DOM.Events as PE
import Signal.Channel as Channel
-------------------------------------------------------------------------------


type State =
  { route :: Route
  , timer :: TimerState
  }


-------------------------------------------------------------------------------
type TimerState =
  { timerTotal :: TD.Seconds
  , chimeTotal :: TD.Seconds
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

           | TimerEvent TimerEvent


-------------------------------------------------------------------------------
data TimerEvent = PlusTimerMinute
                | MinusTimerMinute

                | PlusChimeMinute
                | MinusChimeMinute


-------------------------------------------------------------------------------
type Effects eff =
  ( channel :: Channel.CHANNEL
  , exception :: EX.EXCEPTION
  , dom :: DOM.DOM
  , history :: DOMHT.HISTORY
  | eff
  )
