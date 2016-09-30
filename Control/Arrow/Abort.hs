module Control.Arrow.Abort (
    module Control.Arrow.Abort.Class
    , AbortT(..)
    , runAbortT
) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Category
import Control.Arrow
import Control.Arrow.Transformer
import Control.Arrow.Abort.Class
import Util

import Data.Typeable
import Data.Dynamic
import Control.Exception

newtype AbortT v a b c = AbortT { unwrapAbortT :: a b (Either v c) }

runAbortT :: Arrow a => AbortT v a b v -> a b v;
runAbortT = (>>> arr (either id id)) . unwrapAbortT;

instance ArrowTransformer (AbortT v) where
  lift = AbortT . (>>> arr Right)
  tmap f = AbortT . f . unwrapAbortT

instance ArrowChoice a => Category (AbortT v a) where
  id = AbortT (arr Right)
  AbortT f . AbortT g = AbortT (right f . g >>> arr join)

instance ArrowChoice a => Arrow (AbortT v a) where
  arr = AbortT . arr . liftM Right
  first  = AbortT . (>>> arr (uncurry (liftM2 (,)))) . (*** arr Right) . unwrapAbortT
  second = AbortT . (>>> arr (uncurry (liftM2 (,)))) . (arr Right ***) . unwrapAbortT

instance (ArrowChoice a, ArrowApply a) => ArrowApply (AbortT v a) where
  app = AbortT (arr unwrapAbortT *** id >>> app)

instance (ArrowChoice a, ArrowLoop a, Typeable v) => ArrowLoop (AbortT v a) where
  loop (AbortT a) = AbortT (loop (a >>> (id +++ arr fst) &&& (arr (throw . toDyn) ||| arr snd)))

instance (ArrowChoice r) => ArrowAbort v (AbortT v r) where
  abort = AbortT (arr Left)

instance (ArrowChoice a, ArrowTransformer t, Arrow (t (AbortT v a))) => ArrowAbort v (t (AbortT v a)) where
  abort = lift abort
