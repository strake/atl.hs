{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.Except (
    module Control.Arrow.Except.Class
  , ExceptT(..)
  , Except
  , runExcept
) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Except.Class
import Util

newtype ExceptT e a b c = ExceptT { runExceptT :: a b (Either e c) }

type Except e = ExceptT e (->)

runExcept :: Except e a b -> a -> Either e b
runExcept = runExceptT

instance ArrowTrans (ExceptT e) where
  lift = ExceptT . (>>> arr Right)
  tmap f = ExceptT . f . runExceptT

instance ArrowChoice a => Category (ExceptT e a) where
  id = ExceptT (arr Right)
  ExceptT f . ExceptT g = ExceptT (right f . g >>> arr join)

instance ArrowChoice a => Arrow (ExceptT e a) where
  arr = ExceptT . arr . liftM Right
  first  = ExceptT . (>>> arr (uncurry (liftM2 (,)))) . (*** arr Right) . runExceptT
  second = ExceptT . (>>> arr (uncurry (liftM2 (,)))) . (arr Right ***) . runExceptT

instance (ArrowChoice a, ArrowApply a) => ArrowApply (ExceptT e a) where
  app = ExceptT (arr runExceptT *** id >>> app)

instance (ArrowChoice a, ArrowLoop a) => ArrowLoop (ExceptT e a) where
    loop (ExceptT f) = ExceptT (loop (f >>> arr go))
        where go x = (fmap fst x, snd (fromRight x))

instance ArrowChoice a => ArrowError e (ExceptT e a) where
  throwError = ExceptT (arr Left)
  catchError (ExceptT a) handler = ExceptT $ proc x -> do
      y <- a -< x
      case y of
          Right res -> returnA -< Right res
          Left  err -> runExceptT handler -< err
