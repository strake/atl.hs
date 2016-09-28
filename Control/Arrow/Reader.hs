module Control.Arrow.Reader (module Control.Arrow.Reader.Class, ReaderT (..), withReaderT) where

import Prelude hiding ((.), id);

import Control.Monad;
import Control.Category;
import Control.Arrow;
import Control.Arrow.Transformer;
import Control.Arrow.Reader.Class;
import Util;

newtype ReaderT r s a b = ReaderT { runReaderT :: s (a, r) b };

instance ArrowTransformer (ReaderT r) where {
  lift = ReaderT . (<<< arr fst);
  tmap f = ReaderT . f . runReaderT;
};

instance (Arrow s) => Category (ReaderT r s) where {
  id = ReaderT (arr fst);
  ReaderT f . ReaderT g = ReaderT (f <<< g *** id <<< id &&& arr snd);
};

instance (Arrow s) => Arrow (ReaderT r s) where {
  arr = lift . arr;
  first  = ReaderT . (<<< swap_snds_A) . (*** id) . runReaderT;
};

instance (ArrowApply s) => ArrowApply (ReaderT r s) where {
  app = ReaderT (arr (\ ((ReaderT f, x), r) -> (f, (x, r))) >>> app);
};

instance (ArrowZero s) => ArrowZero (ReaderT r s) where {
  zeroArrow = ReaderT zeroArrow;
};

instance (ArrowPlus s) => ArrowPlus (ReaderT r s) where {
  ReaderT f <+> ReaderT g = ReaderT (f <+> g);
};

instance (ArrowChoice s) => ArrowChoice (ReaderT r s) where {
  left (ReaderT x) = ReaderT $ let {
                       f :: (Either a b, r) -> Either (a, r) b;
                       f (Left  x, r) = Left (x, r);
                       f (Right y, r) = Right y;
                     } in arr f >>> left x;
};

instance (ArrowLoop s) => ArrowLoop (ReaderT r s) where {
  loop = ReaderT . loop . (<<< swap_snds_A) . runReaderT;
};

instance (Arrow s) => ArrowReader r (ReaderT r s) where {
  ask   = ReaderT (arr snd);
  local = withReaderT . arr;
};

instance (Arrow s, ArrowTransformer xT, Arrow (xT (ReaderT r s))) => ArrowReader r (xT (ReaderT r s)) where {
  ask   = lift ask;
  local (f :: r -> r) =
    let local' (a :: s r r) = tmap (withReaderT a :: ∀ a b. ReaderT r s a b →  ReaderT r s a b);
    in local' (arr f);
};

withReaderT :: Arrow s => s q r -> ReaderT r s a b -> ReaderT q s a b;
withReaderT a = ReaderT . (<<< id *** a) . runReaderT;
