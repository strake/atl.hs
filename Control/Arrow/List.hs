{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , UndecidableInstances
  #-}

module Control.Arrow.List (
  -- * The ListT arrow transformer
    ListT(..)
  -- * The pure List arrow
  , List
  , runList
  -- * Re-exports
  , module Control.Arrow.List.Class
) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Hoist
import Control.Arrow.List.Class
import Util

import Data.Tuple

-- | Embeds a multiple-output arrow.
newtype ListT a b c = ListT { runListT :: a b [c] }

-- | Pure List arrow.
type List = ListT (->)

-- | Runs a pure List arrow.
runList :: List a b -> a -> [b]
runList = runListT

instance ArrowTrans ListT where
    lift a = ListT (a >>^ return)

instance ArrowHoist ListT where
    hoistA f (ListT a) = ListT (f a)

instance ArrowChoice a => Category (ListT a) where
    id = ListT (arr return)
    ListT f . ListT g = ListT $ proc x -> do
            ys <- g -< x
            go -< (ys, [])
        where go = proc (lst, acc) -> case lst of
                []     -> returnA -< acc
                (x:xs) -> do
                    zs <- f -< x
                    go -< (xs, zs ++ acc)

instance ArrowChoice a => Arrow (ListT a) where
    arr f = ListT (arr (return . f))
    first (ListT f) = ListT $ proc (x, y) -> do
        xs <- f -< x
        returnA -< zip xs (repeat y)

instance (ArrowChoice a, ArrowApply a) => ArrowApply (ListT a) where
    app = ListT (first (arr runListT) >>> app)

instance ArrowChoice a => ArrowZero (ListT a) where
    zeroArrow = ListT (constA [])

instance ArrowChoice a => ArrowPlus (ListT a) where
    ListT f <+> ListT g = ListT $ proc x -> do
        xs <- f -< x
        ys <- g -< x
        returnA -< xs ++ ys

instance ArrowChoice a => ArrowChoice (ListT a) where
    left (ListT f) = ListT $ proc ex -> case ex of
            Left  x -> do
                xs <- f -< x
                returnA -< map Left xs
            Right x -> returnA -< [Right x]

-- todo: ArrowLoop implementation

instance ArrowChoice a => ArrowList (ListT a) where
    arrL = ListT . arr
    mapL f (ListT a) = ListT (a >>^ f)
