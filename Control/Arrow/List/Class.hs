{-# LANGUAGE FlexibleInstances #-}

module Control.Arrow.List.Class (
  -- * The ArrowList class
    ArrowList
  , arrL
  , mapL
) where

import Control.Arrow
import Control.Applicative

-- | A multiple output arrow
class Arrow a => ArrowList a where
    -- | Wraps a pure function into an @ArrowList@.
    arrL :: (b  -> [c])  -- ^ The pure function to wrap
         -> a b c

    -- | Applies a pure function to every output of a given arrow.
    mapL :: ([c] -> [d])  -- ^ The mapping function
         -> a b c         -- ^ The arrow to map
         -> a b d

instance ArrowList (Kleisli []) where
    arrL = Kleisli . arr
    mapL f (Kleisli k) = Kleisli (\ x -> f (k x))
