module Zenyatta.Timer
   ( view
   ) where


-------------------------------------------------------------------------------
import Pux.DOM.HTML as PH
import Zenyatta.Types as T
import Text.Smolder.Markup (text)
-------------------------------------------------------------------------------


--Probably take a timer state?
view :: T.State -> PH.HTML T.Event
view _  = text "timer"
