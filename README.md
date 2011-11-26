Haskell Hold 'em
================

Haskell Hold 'em is a no-limit Texas Hold 'em game implemented as a
literate Haskell program. You can read it to learn the rules of the
game and then run it to try the rules in practice.

The Name
--------

Haskell is a small city in Texas.

Build Docs
----------

TODO: How to generate LaTeX

Build Source
------------

See dependencies in haskell-holdem.cabal

  runhaskell Setup configure --enable-tests --user
  runhaskell Setup build
  runhaskell Setup test
  runhaskell Setup install # optional

Running Unit Tests
------------------

Either do as described above, or

  runhaskell -i./src -i./tests tests/Test.hs

License
-------

This work is licensed under a Creative Commons Attribution-ShareAlike
3.0 Unported License.
Copyright (c) Björn Edström <be@bjrn.se> 2011
See LICENSE for more information.
