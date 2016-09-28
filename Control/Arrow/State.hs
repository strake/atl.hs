module Control.Arrow.State (module Control.Arrow.State.Class, StateT (..)) where

import Prelude hiding ((.), id);

import Control.Arrow;
import Control.Arrow.Transformer;
import Control.Arrow.State.Class;
import Control.Category;
import Util;

newtype StateT s r a b = StateT { runStateT :: r (a, s) (b, s) };

instance ArrowTransformer (StateT s) where {
  lift = StateT . (*** id);
  tmap f = StateT . f . runStateT;
};

instance (Category r) => Category (StateT s r) where {
  id = StateT id;
  StateT x . StateT y = StateT (x . y);
};

instance (Arrow r) => Arrow (StateT s r) where {
  arr = StateT . arr . (*** id);
  first = StateT . (>>>) swap_snds_A . (<<<) swap_snds_A . (*** id) . runStateT;
};

instance (ArrowApply r) => ArrowApply (StateT s r) where {
  app = StateT $ arr (\ ((StateT f, x), s) -> (f, (x, s))) >>> app;
};

instance (ArrowZero r) => ArrowZero (StateT s r) where {
  zeroArrow = StateT zeroArrow;
};

instance (ArrowPlus r) => ArrowPlus (StateT s r) where {
  StateT f <+> StateT g = StateT (f <+> g);
};

instance (ArrowChoice r) => ArrowChoice (StateT s r) where {
  left (StateT x) = StateT $ let {
                      f (Left  x, s) = Left  (x, s);
                      f (Right y, s) = Right (y, s);
                      g (Left  (x, s)) = (Left  x, s);
                      g (Right (y, s)) = (Right y, s);
                    } in arr f >>> left x >>> arr g;
};

instance (ArrowLoop r) => ArrowLoop (StateT s r) where {
  loop = StateT . loop . (>>> swap_snds_A) . (<<< swap_snds_A) . runStateT;
};

instance (Arrow r) => ArrowState s (StateT s r) where {
  get = StateT $ arr $ \ (_, s) -> (s,  s);
  put = StateT $ arr $ \ (s, _) -> ((), s);
};

instance (Arrow r, ArrowTransformer xT, Arrow (xT (StateT s r))) => ArrowState s (xT (StateT s r)) where {
  get = lift get;
  put = lift put;
};
