-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module Control.Arrow.Transformer where

import Control.Arrow

class ArrowTransformer t where
  lift :: Arrow a => a b c -> t a b c
  tmap :: (Arrow a1, Arrow a2) => (forall b c. a1 b c -> a2 b c) -> t a1 b c -> t a2 b c
