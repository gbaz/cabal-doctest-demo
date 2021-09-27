# cabal-doctest-demo

This repo is a companion to https://github.com/haskell/cabal/pull/7688 which shows how the code-generators field can be used.

This repo gives a package that has two different doctest runners built on top of that functionality. One wraps up the doctest program with a wrapper generated from the ghc options (and so which does not rely on a ghc environment file to bring deps into scope, but rather extracts them directly form the build-depends). The other uses the lexer from https://github.com/phadej/cabal-extras/tree/master/cabal-docspec and generates a fully static tasty testsuite from the doctest comments in the source lib.

The static testsuite is very much a proof of concept, since not all doctests translate cleanly into statically compiled (rather than ghci-driven) tests. However, it does point the way to a much more lightweight approach to such tests than anything existing, which hopefully can be further improved in the future and solidify into a new standard approach.
