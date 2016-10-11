{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.Store (
  -- * The StoreT arrow transformer
    StoreT(..)

  -- * The pure State arrow
  , Store
  , runStore

  -- * Re-exports
  , module Control.Arrow.Store.Class
) where

import Prelude hiding ((.), id)

import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Hoist
import Control.Arrow.Store.Class
import Control.Category
import Util

-- | A context-dependent computation.
-- newtype StoreT s a b c = StoreT { runStoreT :: a (s -> b, s) (s -> c, s) }
-- newtype StoreT s a b c = StoreT { runStoreT :: a (s -> b) (s -> c), pos :: s }
-- newtype StoreT s a b c = StoreT { runStoreT :: a (s -> b) c, pos :: s }
-- newtype StoreT s a b c = StoreT { runStoreT :: a s b -> a s c }
-- newtype StoreT s a b c = StoreT { runStoreT :: a (s -> b) (s -> c) }
-- newtype StoreT s a b c = StoreT { runStoreT :: (a s b, s) (a s c, s) }


--     CokleisliT a (Store s) b c
-- <=> a (Store s b) c
-- <=> a ((s -> b), s) c
newtype StoreT s a b c = StoreT { runStoreT :: a (s -> b, s) c }

-- | The pure Store arrow.
type Store s = StoreT s (->)

-- | Runs a pure context-dependent computation.
runStore :: Store s a b -> (s -> a, s) -> b
runStore = runStoreT

instance ArrowTrans (StoreT s) where
    lift a = StoreT (uncurry ($) ^>> a)

instance ArrowHoist (StoreT s) where
    hoistA f (StoreT a) = StoreT (f a)

instance Arrow a => Category (StoreT s a) where
    id = StoreT (arr $ uncurry ($))
    StoreT a . StoreT b = StoreT $ proc (f, s) -> do
        x <- b -< (f, s)
        a -< (const x, s)

instance Arrow a => Arrow (StoreT s a) where
    arr f = StoreT $ proc (g, s) -> do
        returnA -< f (g s)

    first (StoreT a) = StoreT $ proc (f, s) -> do
        (x, y) <- returnA -< f s
        x' <- a -< (const x, s)
        returnA -< (x', y)

instance ArrowZero a => ArrowZero (StoreT s a) where
    zeroArrow = lift zeroArrow

instance ArrowPlus a => ArrowPlus (StoreT s a) where
    StoreT a <+> StoreT b = StoreT (a <+> b)

instance ArrowChoice a => ArrowChoice (StoreT s a) where
    left (StoreT a) = StoreT $ proc (f, s) -> do
        case f s of
            Right x -> returnA -< Right x
            Left  x -> do
                y <- a -< (const x, s)
                returnA -< Left y

instance ArrowApply a => ArrowApply (StoreT s a) where
    app = StoreT $ proc (f, s) -> do
        (StoreT a, x) <- returnA -< f s
        app -< (a, (const x, s))

instance Arrow a => ArrowStore s (StoreT s a) where
    pos = StoreT (arr snd)
    peek (StoreT a) = StoreT $ proc (f, s) ->
        a -< (const (), f s)
