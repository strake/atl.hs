{-# LANGUAGE
    Arrows
  , FunctionalDependencies
  , FlexibleInstances
  #-}

module Control.Arrow.Writer.Class (
  -- * The ArrowWriter class
    ArrowWriter
  , writer
  , tell
  , listen
  , pass

  -- * Helper functions
  , censor
  , listenA
  , listens
) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Category
import Control.Arrow.Kleisli
import qualified Control.Monad.Writer as M

import Data.Monoid

-- | An arrow which outputs a result evaluated along the computation.
class Arrow a => ArrowWriter w a | a -> w where
    -- | Wraps a pure function into an arrow.
    writer :: (b -> (c, w))  -- ^ The pure function to wrap
           -> a b c

    writer f = proc x -> do
        (y, w) <- returnA -< f x
        tell -< w
        returnA -< y

    -- | Overwrites the current state with a given value.
    tell :: a w ()
    tell = writer (\ w -> ((), w))

    -- | Runs a computation and returns the output.
    listen :: a b c       -- ^ The computation to run
           -> a b (c, w)

    -- | Runs an action and applies the output function to the current state.
    pass :: a b (c, w -> w)  -- ^ The arrow which outputs the result and the altering function
         -> a b c


-- | Runs a computation and applies the output to a pure function.
censor :: ArrowWriter w a
       => (w -> w)  -- ^ The pure function to apply
       -> a b c     -- ^ The computation to run
       -> a b c

censor f a = pass $ proc x -> do
    y <- a -< x
    returnA -< (y, f)


-- | Runs a computation depending on the state.
listenA :: ArrowWriter w a
        => a w c       -- ^ the computation to run
        -> a b (b, c)

listenA a = proc x -> do
    (_, w) <- listen id -< ()
    c <- a -< w
    returnA -< (x, c)


-- | Pure equivalent of @listenA@.
listens :: ArrowWriter w a => (w -> c) -> a b (b, c)
listens = listenA . arr

instance (Monoid w, Monad m) => ArrowWriter w (Kleisli (M.WriterT w m)) where
    tell = arrK M.tell
    listen k = Kleisli (\ x -> M.listen (runKleisli k x))
    pass k = Kleisli (\ x -> M.pass (runKleisli k x))
