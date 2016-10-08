{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  , InstanceSigs
  #-}

module Control.Arrow.Cont where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Util

import Control.Arrow.Cont.Class

newtype ContT r a b c = ContT { runContT :: a c r -> a b r }

evalContT :: ArrowApply a => ContT r a b r -> a b r
evalContT (ContT f) = f id

type Cont r = ContT r (->)

runCont :: Cont r a b -> (b -> r) -> a -> r
runCont = runContT

evalCont :: Cont r a r -> a -> r
evalCont = evalContT

instance ArrowTrans (ContT r) where
    lift = ContT . (>>>)

instance Arrow a => Category (ContT r a) where
    id = ContT id
    ContT f . ContT g = ContT (g . f)

instance ArrowApply a => Arrow (ContT r a) where
    arr = ContT . (^>>)
    first (ContT f) = ContT $ \ a -> proc (x, y) ->
        app -< (f (id &&& constA y >>> a), x)

instance (ArrowApply a, ArrowZero a) => ArrowZero (ContT r a) where
    zeroArrow = lift zeroArrow

instance (ArrowApply a, ArrowPlus a) => ArrowPlus (ContT r a) where
    ContT a <+> ContT b = ContT (a &&& b >>> uncurry (<+>))

instance ArrowApply a => ArrowApply (ContT r a) where
    app = ContT $ \ a -> proc (b, x) -> app -< (runContT b a, x)

resetT :: ArrowApply a => ContT r a b r -> ContT r' a b r
resetT = lift . evalContT

shiftT :: ArrowApply a => (a b r -> ContT r a c r) -> ContT r a c b
shiftT f = ContT (evalContT . f)

reset :: Cont r a r -> Cont r' a r
reset = resetT

shift :: ((a -> r) -> Cont r b r) -> Cont r b a
shift = shiftT

instance ArrowApply a => ArrowCont r (ContT r a) where
    callCC f = ContT (arr (runContT . f . lift) &&& id >>> app)
