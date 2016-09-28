module Control.Arrow.Abort (module Control.Arrow.Abort.Class, AbortT (..), runAbortT) where

import Prelude hiding ((.), id);

import Control.Monad;
import Control.Category;
import Control.Arrow;
import Control.Arrow.Transformer;
import Control.Arrow.Abort.Class;
import Util;

import Data.Typeable;
import Data.Dynamic;
import Control.Exception;

newtype AbortT v r a b = AbortT { unwrapAbortT :: r a (Either v b) };

runAbortT :: Arrow r => AbortT v r a v -> r a v;
runAbortT = (>>> arr (either id id)) . unwrapAbortT;

instance ArrowTransformer (AbortT v) where {
  lift = AbortT . (>>> arr Right);
  tmap f = AbortT . f . unwrapAbortT;
};

instance (ArrowChoice r) => Category (AbortT v r) where {
  id = AbortT (arr Right);
  AbortT f . AbortT g = AbortT (right f . g >>> arr join);
};

instance (ArrowChoice r) => Arrow (AbortT v r) where {
  arr = AbortT . arr . liftM Right;
  first  = AbortT . (>>> arr (uncurry (liftM2 (,)))) . (*** arr Right) . unwrapAbortT;
  second = AbortT . (>>> arr (uncurry (liftM2 (,)))) . (arr Right ***) . unwrapAbortT;
};

instance (ArrowChoice r, ArrowApply r) => ArrowApply (AbortT v r) where {
  app = AbortT (arr unwrapAbortT *** id >>> app);
};

instance (ArrowChoice r, ArrowLoop r, Typeable v) => ArrowLoop (AbortT v r) where {
  loop (AbortT a) = AbortT (loop (a >>> (id +++ arr fst) &&& (arr (throw . toDyn) ||| arr snd)));
};

instance (ArrowChoice r) => ArrowAbort v (AbortT v r) where {
  abort = AbortT (arr Left);
};

instance (ArrowChoice r, ArrowTransformer xT, Arrow (xT (AbortT v r))) => ArrowAbort v (xT (AbortT v r)) where {
  abort = lift abort;
};
