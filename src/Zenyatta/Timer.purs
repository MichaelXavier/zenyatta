module Zenyatta.Timer
   ( view
   , foldp
   ) where


-------------------------------------------------------------------------------
import Data.Int as Int
import Data.Time.Duration as TD
import Pux as P
import Pux.DOM.HTML as PH
import Zenyatta.Prelude as Prelude
import Zenyatta.Types as T
import Data.Newtype (unwrap)
import Pux.DOM.Events (onClick)
import Text.Smolder.HTML (div, h1, nav)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup, parent, text, (!), (#!))
import Zenyatta.Prelude hiding (div)
-------------------------------------------------------------------------------


--Probably take a timer state?
--TODO: break up
view :: T.State -> PH.HTML T.Event
view s  = main ! classes ["main"] $ do
  header
  content
  where
    header = do
      nav ! classes ["header"] $ do
        --TODO: active on state
        div ! classes ["tab-button", "active"] $ do
          text "Timer"
        div ! classes ["tab-button"] $ do
          text "Logs"

    content = do
      div ! classes ["content"] $ do
        mainTimer
        chimeTimer
        timerControls

    mainTimer = do
      div ! classes ["main-timer", "timer-block"] $ do
        div ! classes ["gutter"] $ mempty
        div ! classes ["timer"] $ do
          h1 ! classes ["timer-title"] $ do
            text "Timer"
          div ! classes ["center-block"] $ do
            div ! classes ["minutes"] $ do
              --TODO: disable buttons if running or at minimum
              div ! classes ["minutes-control", "control", "plus"]
                 #! onClick (const (T.TimerEvent T.PlusTimerMinute)) $ mempty
              div ! classes ["minutes-display"] $ do
                text (show timerMinutes)
              div ! classes ["minutes-control", "control", "minus"]
                 #! onClick (const (T.TimerEvent T.MinusTimerMinute)) $ mempty
            div ! classes ["seconds"] $ do
              div ! classes ["seconds-control", "control"] $ mempty
              div ! classes ["seconds-display"] $ do
                text (":" <> timerSecondsPadded)
              div ! classes ["seconds-control", "control"] $ mempty
          div ! classes ["progress-bar"] $ do
            --TODO: could we use purescript-css on the attribute?
            div ! classes ["progress-bar-fill"] ! style "width: 75%;" $ mempty
        div ! classes ["gutter"] $ mempty

    chimeTimer = do
      div ! classes ["chime-timer", "timer-block"] $ do
        div ! classes ["gutter"] $ mempty
        div ! classes ["timer"] $ do
          h1 ! classes ["timer-title"] $ do
            text "Chime"
          div ! classes ["center-block"] $ do
            div ! classes ["minutes"] $ do
              div ! classes ["minutes-control", "control", "plus"]
                 #! onClick (const (T.TimerEvent T.PlusChimeMinute)) $ mempty
              div ! classes ["minutes-display"] $ do
                text "3"
              div ! classes ["minutes-control", "control", "minus"]
                 #! onClick (const (T.TimerEvent T.MinusChimeMinute)) $ mempty
            div ! classes ["seconds"] $ do
              div ! classes ["seconds-control", "control"] $ mempty
              div ! classes ["seconds-display"] $ do
                text ":59"
              div ! classes ["seconds-control", "control"] $ mempty
          div ! classes ["progress-bar"] $ do
            --TODO: could we use purescript-css on the attribute?
            div ! classes ["progress-bar-fill"] ! style "width: 75%;" $ do
              mempty
        div ! classes ["gutter"] $ mempty

    timerControls = do
      div ! classes ["controls"] $ do
        div ! classes ["undo", "control"] $ mempty
        div ! classes ["play", "control"] $ mempty

    timerTotal :: Int
    timerTotal = Int.round (unwrap ts.timerTotal)
    timerMinutes :: Int
    timerMinutes = timerTotal `Prelude.div` 60
    timerSeconds :: Int
    timerSeconds = timerTotal `mod` 60
    timerSecondsPadded
      | timerSeconds >= 10 = show timerSeconds
      | otherwise = "0" <> show timerSeconds
    ts = s.timer

-------------------------------------------------------------------------------
main :: forall e. Markup e -> Markup e
main = parent "main"


-------------------------------------------------------------------------------
--TODO: once we get profunctor lenses >= 3.2 we can easily use lenses here
foldp :: T.TimerEvent -> T.State -> P.EffModel T.State T.Event (T.Effects ())
foldp T.PlusTimerMinute s = P.noEffects
  s { timer = s.timer { timerTotal = s.timer.timerTotal + TD.Seconds 60.0} }
foldp T.MinusTimerMinute s
  | s.timer.timerTotal > minute = P.noEffects
      s { timer = s.timer { timerTotal = s.timer.timerTotal - TD.Seconds 60.0} }
  | otherwise = P.noEffects s
foldp T.PlusChimeMinute s = P.noEffects
  s { timer = s.timer { chimeTotal = s.timer.chimeTotal + TD.Seconds 60.0} }
foldp T.MinusChimeMinute s
  | s.timer.chimeTotal > minute = P.noEffects
      s { timer = s.timer { chimeTotal = s.timer.chimeTotal - TD.Seconds 60.0} }
  | otherwise = P.noEffects s


-------------------------------------------------------------------------------
minute :: TD.Seconds
minute = TD.Seconds 60.0
