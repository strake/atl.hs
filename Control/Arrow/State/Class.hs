{-# LANGUAGE
    Arrows
  , MultiParamTypeClasses
  , FunctionalDependencies
  , UndecidableInstances
  #-}

module Control.Arrow.State.Class where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow
import Control.Arrow.Writer.Class
import Control.Arrow.Reader.Class

class Arrow a => ArrowState s a | a -> s where
    state :: (b -> s -> (c, s)) -> a b c
    state f = proc x -> do
        s <- get -< ()
        (y, s') <- returnA -< f x s
        put -< s'
        returnA -< y

    get :: a () s
    get = state (\ _ s -> (s, s))

    put :: a s ()
    put = state (\ s _ -> ((), s))


gets :: ArrowState s a => a (s -> b) b
gets = proc f -> do
    s <- get -< ()
    returnA -< f s

modify :: ArrowState s a => a (s -> s) ()
modify = proc f -> do
    s <- get -< ()
    put -< f s

set :: ArrowState s a => (s -> s) -> a b b
set f = arr id &&& (arr (const ()) >>> get >>> arr f >>> put) >>> arr fst


instance ArrowState w a => ArrowWriter w a where
    writer = state . (const .)
    tell = put
    listen a = (\ x -> (x, ())) ^>> a *** get
    pass a = a >>> (id *** modify) >>^ fst

instance ArrowState r a => ArrowReader r a where
    reader f = state (\ x r -> (f x r, r))
    ask = get
    local f a = proc x -> do
        s <- get -< ()
        put -< f s
        y <- a -< x
        put -< s
        returnA -< y

instance (ArrowWriter s a, ArrowReader s a) => ArrowState s a where
    get = ask
    put = tell
