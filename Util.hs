module Util where

import Control.Arrow

constA :: Arrow a => b -> a c b
constA = arr . const

swap_snds_A :: Arrow a => a ((b, c), d) ((b, d), c)
swap_snds_A = arr $ \ ((x, y), z) -> ((x, z), y)

fromRight :: Either a b -> b
fromRight (Left _)  = error "Util.fromRight: Left value"
fromRight (Right x) = x

-- Iterate over state-dependent list and apply to each value an arrow
forA :: ArrowChoice a => a () [b] -> a b c -> a () [c]
forA iter action = proc () -> do
        lst <- iter -< ()
        go -< (lst, [])
    where go = proc (lst, acc) -> case lst of
            []     -> returnA -< acc
            (x:xs) -> do
                elt <- action -< x
                go -< (xs, elt:acc)

forA_ :: ArrowChoice a => a () [b] -> a b c -> a () ()
forA_ iter action = proc () -> do
    _ <- forA iter action -< ()
    returnA -< ()


repeatA :: (ArrowChoice a, Integral n) => n -> a b c -> a b [c]
repeatA 0 _ = arr (const [])
repeatA n action = proc x -> go -< (x, n, [])
    where go = proc (x, n, acc) -> case n of
            0 -> returnA -< acc
            i -> do
                elt <- action -< x
                go -< (x, (n - 1), elt:acc)

repeatA_ :: (ArrowChoice a, Integral n) => n -> a b c -> a b ()
repeatA_ n action = proc x -> do
    _ <- repeatA n action -< x
    returnA -< ()
