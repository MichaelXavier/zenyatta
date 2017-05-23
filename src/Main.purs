module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Message (msg)
import Prelude
-------------------------------------------------------------------------------


main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = log msg
