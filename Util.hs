module Util where

import Control.Arrow;

constA :: Arrow r => a -> r b a;
constA = arr . const;

swap_snds_A :: Arrow r => r ((a, b), c) ((a, c), b);
swap_snds_A = arr $ \ ((x, y), z) -> ((x, z), y);
