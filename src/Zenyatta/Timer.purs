module Zenyatta.Timer
   ( view
   , foldp
   ) where


-------------------------------------------------------------------------------
--FIXME: importing CSS hangs rollup
--import CSS as CSS
import Data.Int as Int
import Data.Time.Duration as TD
import Pux as P
import Pux.DOM.Events as PE
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
            div ! classes ["progress-bar-fill"] ! style timerStyle $ mempty
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
                text (show chimeMinutes)
              div ! classes ["minutes-control", "control", "minus"]
                 #! onClick (const (T.TimerEvent T.MinusChimeMinute)) $ mempty
            div ! classes ["seconds"] $ do
              div ! classes ["seconds-control", "control"] $ mempty
              div ! classes ["seconds-display"] $ do
                text (":" <> chimeSecondsPadded)
              div ! classes ["seconds-control", "control"] $ mempty
          div ! classes ["progress-bar"] $ do
            --TODO: could we use purescript-css on the attribute?
            div ! classes ["progress-bar-fill"] ! style chimeStyle $ do
              mempty
        div ! classes ["gutter"] $ mempty

    timerControls = do
      div ! classes ["controls"] $ do
        div
          ! classes ["undo", "control"]
          #! PE.onClick (const (T.TimerEvent T.Reset))
          $ mempty
        case ts.status of
          T.Stopped ->
            div
              ! classes ["play", "control"]
              #! PE.onClick (const (T.TimerEvent T.Start))
              $ mempty
          T.Running ->
            div
              ! classes ["pause", "control"]
              #! PE.onClick (const (T.TimerEvent T.Stop))
              $ mempty

    timerStyle = "width: " <> show timerPct <> "%" -- fromMaybe mempty (CSS.renderedInline (CSS.render (CSS.width (CSS.pct totalPct))))
    timerPct = unwrap ts.timerRemaining / unwrap ts.timerTotal * 100.0

    timerRemaining = Int.round (unwrap ts.timerRemaining)
    timerMinutes = timerRemaining `Prelude.div` 60
    timerSeconds = timerRemaining `mod` 60
    timerSecondsPadded
      | timerSeconds >= 10 = show timerSeconds
      | otherwise = "0" <> show timerSeconds

    chimeStyle = "width: " <> show chimePct <> "%"
    chimePct = unwrap ts.chimeRemaining / unwrap ts.chimeTotal * 100.0

    chimeRemaining = Int.round (unwrap ts.chimeRemaining)
    chimeMinutes = chimeRemaining `Prelude.div` 60
    chimeSeconds = chimeRemaining `mod` 60
    chimeSecondsPadded
      | chimeSeconds >= 10 = show chimeSeconds
      | otherwise = "0" <> show chimeSeconds

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
foldp T.Tick s@{ timer: ts@{ status: T.Running } } = --TODO: stop when at 0
  let timer = ts
        { timerRemaining = tickSeconds ts.timerRemaining
        , chimeRemaining = tickSeconds ts.chimeRemaining
        }
      state = s { timer = timer}
  in P.noEffects state
foldp T.Tick s = P.noEffects s
foldp T.Start s@{timer: ts} = P.noEffects s { timer = ts { status = T.Running } }
foldp T.Stop s@{timer: ts} = P.noEffects s { timer = ts { status = T.Stopped } }
foldp T.Reset s@{timer: ts} =
  let timer = ts
        { timerRemaining = ts.timerTotal
        , chimeRemaining = ts.chimeTotal
        , status = T.Stopped
        }
  in P.noEffects s { timer = timer }


-------------------------------------------------------------------------------
minute :: TD.Seconds
minute = TD.Seconds 60.0


-------------------------------------------------------------------------------
tickSeconds :: TD.Seconds -> TD.Seconds
tickSeconds s
  | s > zero  = s - one
  | otherwise = s
