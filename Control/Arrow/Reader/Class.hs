{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Control.Arrow.Reader.Class (
  -- * The ArrowReader class
    ArrowReader
  , reader
  , ask
  , local

  -- * Helper functions
  , asks
) where

import Control.Arrow
import Control.Arrow.Kleisli

import qualified Control.Monad.Reader as M

-- | An arrow which can read the input state, but cannot modify it
class Arrow a => ArrowReader r a | a -> r where
    -- | Wraps a pure function into an arrow.
    reader :: (b -> r -> c)  -- ^ The pure function to wrap
           -> a b c
    reader f = proc x -> do
        r <- ask -< ()
        returnA -< f x r

    -- | Returns the current state.
    ask :: a () r
    ask = reader (\ _ r -> r)

    -- | Executes an action with a temporary state.
    local :: (r -> r)  -- ^ The function giving the temporary state from the current one.
          -> a b c     -- ^ The arrow to execute in the modified environment.
          -> a b c

-- | @asks@ returns the state processed by a pure function.
asks :: ArrowReader r a => a (r -> b) b
asks = proc f -> do
    r <- ask -< ()
    returnA -< f r

instance Monad m => ArrowReader r (Kleisli (M.ReaderT r m)) where
    ask = liftK M.ask
    local f k = Kleisli (\ x -> M.local f (runKleisli k x))
