module Zenyatta.Prelude
  ( module Prelude
  , module Control.Monad.Eff
  , module Control.Monad.Eff.Class
  , module Data.Maybe
  , module Data.Either
  , noop
  ) where


-------------------------------------------------------------------------------
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Data.Maybe
import Data.Either
import Prelude
-------------------------------------------------------------------------------


noop :: forall f. Applicative f => f Unit
noop = pure unit
