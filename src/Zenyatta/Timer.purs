module Zenyatta.Timer
   ( view
   ) where


-------------------------------------------------------------------------------
import Pux.DOM.HTML as PH
import Zenyatta.Types as T
import Text.Smolder.HTML (div, h1, nav)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup, parent, text, (!))
import Zenyatta.Prelude hiding (div)
-------------------------------------------------------------------------------


--Probably take a timer state?
--TODO: break up
view :: T.State -> PH.HTML T.Event
view _  = main ! classes ["main"] $ do
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
              div ! classes ["minutes-control", "control", "plus"] $ mempty
              div ! classes ["minutes-display"] $ do
                text "3"
              div ! classes ["minutes-control", "control", "minus"] $ mempty
            div ! classes ["seconds"] $ do
              div ! classes ["seconds-control", "control"] $ mempty
              div ! classes ["seconds-display"] $ do
                text ":59"
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
              div ! classes ["minutes-control", "control", "plus"] $ mempty
              div ! classes ["minutes-display"] $ do
                text "3"
              div ! classes ["minutes-control", "control", "minus"] $ mempty
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

-------------------------------------------------------------------------------
main :: forall e. Markup e -> Markup e
main = parent "main"
