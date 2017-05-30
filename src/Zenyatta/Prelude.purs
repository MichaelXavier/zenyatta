module Zenyatta.Prelude
  ( module Prelude
  , module Control.Monad.Eff
  , module Control.Monad.Eff.Class
  , module Control.Monad.Eff.Console
  , module Data.NonEmpty
  , module Data.Maybe
  , module Data.Monoid
  , module Data.Either
  , module Data.Foldable
  , module Data.Traversable
  , noop
  ) where


-------------------------------------------------------------------------------
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Maybe
import Data.Monoid
import Data.Either
import Data.Foldable
import Data.Traversable
import Prelude
-------------------------------------------------------------------------------


noop :: forall f. Applicative f => f Unit
noop = pure unit
