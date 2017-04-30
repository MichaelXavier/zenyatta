module Main where


-------------------------------------------------------------------------------
import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (IntervalId, TIMER, clearInterval, setInterval)
import DOM (DOM)
import Data.Enum (class Enum, pred)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Seconds(Seconds))
import Halogen (SubscribeStatus(..))
import Halogen.VDom.Driver (runUI)
-------------------------------------------------------------------------------


type Effs eff = (timer :: TIMER, exception :: EXCEPTION, dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF, now :: NOW | eff)


main :: Eff (Effs ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component unit body
  pure unit


-------------------------------------------------------------------------------
newtype Remaining a = Remaining a

derive newtype instance ordRemaining :: Ord a => Ord (Remaining a)
derive newtype instance eqRemaining :: Eq a => Eq (Remaining a)
derive newtype instance semiringRemaining :: Semiring a => Semiring (Remaining a)
derive instance newtypeEmailAddress :: Newtype (Remaining a) _

instance enumRemainingSeconds :: Enum (Remaining Seconds) where
  succ (Remaining n) = Just (Remaining (add one n))
  pred (Remaining n)
    | n > one = Just (Remaining (n `sub` one))
    | otherwise = Nothing


-------------------------------------------------------------------------------
-- component, extract
type State = { duration :: Seconds
             , remaining :: Remaining Seconds
             , interval :: Maybe IntervalId
             , timerState :: TimerState
             }

data TimerState = Stopped | Started

type Input = Unit

type Message = String



data Query a = SetInterval IntervalId a
             | Tick a
             | Initialize a
             | Finalize a
             | StartTimer a
             | StopTimer a
             | ResetTimer a


--TODO: set remaining
initialState :: State
initialState = {
    interval: Nothing
  , remaining: Remaining defDuration
  , duration: defDuration
  , timerState: Stopped
  }
  where
    defDuration = Seconds (3.0 :: Number)


render :: State -> H.ComponentHTML Query
render s = HH.div_ $
  [ HH.text (remaining <> "/" <> duration)
  ] <> buttons
  where
    --TODO: is there new tech for destructuring newtypes?
    remaining = case s.remaining of Remaining t -> show t
    duration = show s.duration
    --TODO: is this a rounding error?
    --atEnd = s.remaining == zero
    atEnd = s.remaining < one
    atStart = remaining == duration
    startButton = HH.button [HE.onClick (HE.input_ StartTimer)]
      [ HH.text "Start"
      ]
    resetButton = HH.button [HE.onClick (HE.input_ ResetTimer)]
      [ HH.text "Reset"
      ]
    stopButton = HH.button [HE.onClick (HE.input_ StopTimer)]
      [ HH.text "Stop"
      ]
    buttons = case s.timerState of
      --TODO: reset
      Stopped
        | atEnd -> [resetButton]
        | atStart -> [startButton]
        | otherwise -> [startButton, resetButton]
      Started
        | atEnd -> []
        | otherwise -> [stopButton]


countdown :: Remaining Seconds -> Remaining Seconds
countdown r = fromMaybe (Remaining zero) (pred r)


eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Aff (Effs eff))
eval (ResetTimer next) = do
  H.modify (\s -> s { timerState = Stopped, remaining = Remaining s.duration})
  pure next
eval (StartTimer next) = do
  --TODO: is there a combinator to abstract over pure next
  H.modify (_ { timerState = Started})
  pure next
eval (StopTimer next) = do
  H.modify (_ { timerState = Stopped})
  pure next
eval (SetInterval interval next) = do
  -- TODO: maybe stop old interval?
  cancelCurrentInterval
  H.modify (_ { interval = Just interval})
  pure next
eval (Tick next) = do
  H.liftEff (log "our tick")
  timerState <- H.gets (_.timerState)
  case timerState of
    --TODO: there may be a cleaner way to do this
    Started -> do
      H.modify (\s -> s { remaining = countdown s.remaining})
      remaining <- H.gets _.remaining
      when (remaining == zero) (H.modify (_ { timerState = Stopped}))
    Stopped -> pure unit
  -- count down duration
  pure next
eval (Initialize next) = do
  -- app.query (action Tick)
  --H.action Tick
  -- id makes no sense. why is it a function and not a
  let register tick = do
        log "registering tick with setInterval"
        interval <- H.liftEff (setInterval 1000 (tick Nothing))
        tick (Just interval)
  --TODO: rename
  let hmm mInterval = case mInterval of
        Just interval -> Just (SetInterval interval Listening)
        Nothing -> Just (Tick Listening)

  H.subscribe (H.eventSource register hmm) -- i guess we use Done on finalize?

  --TODO: kick off a timer
  pure next
eval (Finalize next) = do
  cancelCurrentInterval
  H.modify (_ {interval = Nothing})
  pure next


-------------------------------------------------------------------------------
cancelCurrentInterval :: forall eff. H.ComponentDSL State Query Message (Aff (Effs eff)) Unit
cancelCurrentInterval = do
  interval <- H.gets _.interval
  H.liftEff (maybe (pure unit) clearInterval interval)


-------------------------------------------------------------------------------
component :: forall eff. H.Component HH.HTML Query Input Message (Aff (Effs eff))
component =  H.lifecycleComponent
  { initialState: const initialState
  , render: render
  , eval: eval
  , receiver: const Nothing
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }
