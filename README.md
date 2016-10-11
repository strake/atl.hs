atl - Arrow Transformer Library
===============================

atl is a Haskell library that implements arrow transformers based on the popular
mtl library.

## Features

atl features 7 arrow transformers which you might recognize from mtl:

- ContT
- ExceptT
- ListT
- ReaderT
- RWST
- StateT
- WriterT

... and their pure equivalents (arrows transformers on `(->)`).

atl also ports comonads from the comonad package:

- StoreT

## Installing

You can install atl either by cloning the repository

    $ git clone http://github.com/felko/atl.git
    $ cd atl
    $ cabal install

... or by using Hackage:

    $ cabal install atl

## License

atl is under the BSD3 licence.

Copyright M Farkas-Dyck (c) 2016
