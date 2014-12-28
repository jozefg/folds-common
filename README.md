## `folds-common`

[![build status][build]][travis]

One nice thing about the [`foldl`][foldl] library is that it comes
with a "Batteries Included" suite of common folds to be used with its
combinators.

This package ports a lot of those combinators to use the
[`folds`][folds] package. Since `foldl` only provides left folds,
this package provides many additional folds that take advantage of
the laziness and associativity of a lot of folding operations.

[folds]: http://hackage.haskell.org/package/folds
[foldl]: http://hackage.haskell.org/package/foldl
[build]: https://travis-ci.org/jozefg/folds-common.svg?branch=master
[travis]: https://travis-ci.org/jozefg/folds-common
