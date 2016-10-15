{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FlexibleInstances
  , FlexibleContexts
  #-}

module Control.Arrow.Except (
  -- * The ExceptT arrow transformer
    ExceptT(..)
  , fromExceptT

  -- * The pure Except arrow
  , Except
  , runExcept
  , fromExcept

  -- * Re-exports
  , module Control.Arrow.Except.Class
) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Category
import Control.Arrow
import Control.Arrow.Trans
import Control.Arrow.Hoist
import Control.Arrow.Except.Class
import Util

fromEither :: b -> Either a b -> b
fromEither x (Left  _) = x
fromEither _ (Right x) = x

-- | A failable computation.
newtype ExceptT e a b c = ExceptT { runExceptT :: a b (Either e c) }

-- | Runs a computation and gives a default value in case of failure.
fromExceptT :: Arrow a => ExceptT e a b c -> c -> a b c
fromExceptT a d = runExceptT a >>^ fromEither d

-- | Pure ExceptT
type Except e = ExceptT e (->)

-- | Runs an Except arrow.
runExcept :: Except e a b -> a -> Either e b
runExcept = runExceptT

-- | Runs a computation and gives a default value in case of failure.
fromExcept :: Except e a b -> b -> a -> b
fromExcept = fromExceptT

instance ArrowTrans (ExceptT e) where
    lift = ExceptT . (>>> arr Right)

instance ArrowHoist (ExceptT e) where
    hoistA f (ExceptT a) = ExceptT (f a)

instance ArrowChoice a => Category (ExceptT e a) where
  id = ExceptT (arr Right)
  ExceptT f . ExceptT g = ExceptT (right f . g >>> arr join)

instance ArrowChoice a => Arrow (ExceptT e a) where
  arr = ExceptT . arr . liftM Right
  first  = ExceptT . (>>> arr (uncurry (liftM2 (,)))) . (*** arr Right) . runExceptT
  second = ExceptT . (>>> arr (uncurry (liftM2 (,)))) . (arr Right ***) . runExceptT

instance (ArrowChoice a, ArrowZero a) => ArrowZero (ExceptT e a) where
    zeroArrow = lift zeroArrow

instance (ArrowChoice a, ArrowPlus a) => ArrowPlus (ExceptT e a) where
    ExceptT a <+> ExceptT b = ExceptT (a <+> b)

instance ArrowChoice a => ArrowChoice (ExceptT e a) where
    left (ExceptT a) = ExceptT $ proc ex -> do
        case ex of
            Right x -> returnA -< Right (Right x)
            Left  x -> do
                ey <- a -< x
                case ey of
                    Left err -> returnA -< Left err
                    Right y  -> returnA -< Right (Left y)


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
