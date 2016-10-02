{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Control.Arrow.Cokleisli (
    module Control.Arrow
  , module Control.Comonad
  , ArrowCokleisli
  , arrCK, liftCK
) where

import Prelude hiding ((.), id)

import Control.Arrow (Arrow, Kleisli(..), runKleisli)
import Control.Arrow.Trans
import Control.Category
import Control.Comonad
import Util

import Data.Monoid

class (Comonad w, Arrow k) => ArrowCokleisli w k | k -> w where
    arrCK :: (w a -> b) -> k a b

liftCK :: ArrowCokleisli w k => b -> k a b
liftCK = arrCK . const

instance Comonad w => ArrowCokleisli w (Cokleisli w) where
    arrCK = Cokleisli

instance (Comonad w, ArrowCokleisli w k, ArrowTrans t, Arrow (t k)) => ArrowCokleisli w (t k) where
    arrCK = lift . arrCK
