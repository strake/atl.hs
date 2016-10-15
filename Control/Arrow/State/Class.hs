{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , UndecidableInstances
  #-}

module Control.Arrow.State.Class (
  -- * The ArrowState class
    ArrowState
  , state
  , get
  , put

  -- * Helper functions
  , gets
  , modify
) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Control.Arrow.Kleisli
import Control.Arrow.Writer.Class
import Control.Arrow.Reader.Class
import qualified Control.Monad.State as M

-- | An arrow that contains a modifiable state
class Arrow a => ArrowState s a | a -> s where
    -- Wraps a pure function into an arrow.
    state :: (b -> s -> (c, s))  -- ^ The pure function to wrap
          -> a b c

    state f = proc x -> do
        s <- get -< ()
        (y, s') <- returnA -< f x s
        put -< s'
        returnA -< y

    -- | Returns the current state.
    get :: a () s
    get = state (\ _ s -> (s, s))

    -- | Overwrites the current state with the given value.
    put :: a s ()
    put = state (\ s _ -> ((), s))


-- | Returns the current state applied to a pure function.
gets :: ArrowState s a => a (s -> b) b
gets = proc f -> do
    s <- get -< ()
    returnA -< f s

-- | Applies a pure function to the current state.
modify :: ArrowState s a => a (s -> s) ()
modify = proc f -> do
    s <- get -< ()
    put -< f s


instance ArrowState w a => ArrowWriter w a where
    writer = state . (const .)
    tell = put
    listen a = (\ x -> (x, ())) ^>> a *** get
    pass a = a >>> (id *** modify) >>^ fst

instance ArrowState r a => ArrowReader r a where
    reader f = state (\ x r -> (f x r, r))
    ask = get
    local f a = proc x -> do
        s <- get -< ()
        put -< f s
        y <- a -< x
        put -< s
        returnA -< y

instance Monad m => ArrowState s (Kleisli (M.StateT s m)) where
    get = liftK M.get
    put = arrK M.put
