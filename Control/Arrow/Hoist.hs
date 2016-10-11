{-# LANGUAGE
    RankNTypes
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.Hoist (
  -- * The ArrowHoist class
    ArrowHoist
  , hoistA
) where

import Control.Arrow
import Control.Arrow.Trans

class ArrowHoist t where
    -- | Yields from any arrow homomorphism from @a@ to @a'@ another arrow homomorphisme from @t a@ to @t a'@.
    hoistA :: (Arrow a, Arrow a') => (forall b c. a b c -> a' b c) -> t a b c -> t a' b c
