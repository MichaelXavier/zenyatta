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
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (IntervalId, TIMER, clearInterval, setInterval)
import DOM (DOM)
import Data.Maybe (Maybe(..), maybe)
import Halogen (SubscribeStatus(..))
import Halogen.VDom.Driver (runUI)
-------------------------------------------------------------------------------


type MyEffects eff = (timer :: TIMER, exception :: EXCEPTION, dom :: DOM, console :: CONSOLE, avar :: AVAR, ref :: REF)

main :: Eff ( avar :: AVAR , ref :: REF , exception :: EXCEPTION , dom :: DOM , console :: CONSOLE , timer :: TIMER) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component unit body
  pure unit


-------------------------------------------------------------------------------
-- component, extract
type State = { msg :: String
             , interval :: Maybe IntervalId
             }

type Input = Unit

type Message = String



--TODO: think i want a lifecycle component
data Query a = ChangeMessage String a
             | SetInterval IntervalId a
             | Tick a
             | Initialize a
             | Finalize a


initialState :: State
initialState = {msg: "init", interval: Nothing}


render :: State -> H.ComponentHTML Query
render s = HH.div_
  [ HH.text s.msg
  , HH.button [HE.onClick (HE.input_ (ChangeMessage "changed"))]
      [ HH.text "ChangeMessage"
      ]
  ]



eval :: forall eff. Query ~> H.ComponentDSL State Query Message (Aff (MyEffects eff))
eval (SetInterval interval next) = do
  -- TODO: maybe stop old interval?
  cancelCurrentInterval
  H.modify (_ { interval = Just interval})
  pure next
eval (ChangeMessage newMsg next) = do
  H.modify (_ { msg = newMsg})
  pure next
eval (Tick next) = do
  H.liftEff (log "our tick")
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
cancelCurrentInterval :: forall eff. H.ComponentDSL State Query Message (Aff (MyEffects eff)) Unit
cancelCurrentInterval = do
  interval <- H.gets _.interval
  H.liftEff (maybe (pure unit) clearInterval interval)


-------------------------------------------------------------------------------
component :: forall eff. H.Component HH.HTML Query Input Message (Aff (MyEffects eff))
component =  H.lifecycleComponent
  { initialState: const initialState
  , render: render
  , eval: eval
  , receiver: const Nothing
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }
