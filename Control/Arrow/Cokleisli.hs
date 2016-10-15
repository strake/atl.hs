{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Control.Arrow.Cokleisli (
  -- * The ArrowCokleisli class
    ArrowCokleisli
  , arrCK, liftCK

  -- * Re-exports
  , module Control.Arrow
  , module Control.Comonad
) where

import Prelude hiding ((.), id)

import Control.Arrow (Arrow, Kleisli(..), runKleisli)
import Control.Arrow.Trans
import Control.Category
import Control.Comonad
import Util

import Data.Monoid

-- | An arrow embedding a comonadic action.
class (Comonad w, Arrow k) => ArrowCokleisli w k | k -> w where
    -- | Wraps a comonadic action into the arrow.
    arrCK :: (w a -> b) -> k a b

-- | Wraps a pure value into an arrow.
liftCK :: ArrowCokleisli w k => b -> k a b
liftCK = arrCK . const

instance Comonad w => ArrowCokleisli w (Cokleisli w) where
    arrCK = Cokleisli

instance (Comonad w, ArrowCokleisli w k, ArrowTrans t, Arrow (t k)) => ArrowCokleisli w (t k) where
    arrCK = lift . arrCK
