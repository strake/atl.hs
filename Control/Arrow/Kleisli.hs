{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Control.Arrow.Kleisli (
  -- * The ArrowKleisli class
    ArrowKleisli
  , arrK, liftK

  -- * Re-exports
  , module Control.Arrow
) where

import Prelude hiding ((.), id)

import Control.Arrow (Arrow, Kleisli(..), runKleisli)
import Control.Arrow.Trans
import Control.Category
import Util

import Data.Monoid

-- | An arrow embedding a monadic action.
class (Monad m, Arrow k) => ArrowKleisli m k | k -> m where
    -- | Wraps a monadic action into the arrow.
    arrK :: (a -> m b) -> k a b

-- | Lifts a monadic value into an arrow.
liftK :: ArrowKleisli m k => m b -> k a b
liftK = arrK . const

instance Monad m => ArrowKleisli m (Kleisli m) where
    arrK = Kleisli

instance (Monad m, ArrowKleisli m k, ArrowTrans t, Arrow (t k)) => ArrowKleisli m (t k) where
    arrK = lift . arrK
