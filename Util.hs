module Util where

import Control.Arrow

constA :: Arrow a => b -> a c b
constA = arr . const

swap_snds_A :: Arrow a => a ((b, c), d) ((b, d), c)
swap_snds_A = arr $ \ ((x, y), z) -> ((x, z), y)

fromRight :: Either a b -> b
fromRight (Left _)  = error "Util.fromRight: Left value"
fromRight (Right x) = x
