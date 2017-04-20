module Main where


-------------------------------------------------------------------------------
import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Halogen.VDom.Driver (runUI)
-------------------------------------------------------------------------------


main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component unit body
  pure unit


-------------------------------------------------------------------------------
-- component, extract
type State = String

type Input = Unit

type Message = String


data Query a = ChangeMessage String a


initialState :: State
initialState = "init"


render :: State -> H.ComponentHTML Query
render msg = HH.div_
  [ HH.text msg
  , HH.button [HE.onClick (HE.input_ (ChangeMessage "changed"))]
      [ HH.text "ChangeMessage"
      ]
  ]



eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval (ChangeMessage newMsg next) = do
  H.put newMsg
  pure next


component :: forall m. H.Component HH.HTML Query Input Message m
component =  H.component
  { initialState: const initialState
  , render: render
  , eval: eval
  , receiver: const Nothing
  }
