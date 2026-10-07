# The `exception-transformers` Package  [![Hackage](https://img.shields.io/hackage/v/exception-transformers.svg)](https://hackage.haskell.org/package/exception-transformers) [![Actions Status: haskell-ci](https://github.com/mainland/exception-transformers/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/exception-transformers/actions?query=workflow%3Ahaskell-ci)

Provides type classes, a monad and a monad transformer that support unchecked extensible exceptions as well as asynchronous exceptions. It is compatible with the [`transformers`](https://hackage.haskell.org/package/transformers) package.

The [`exceptions`](https://hackage.haskell.org/package/exceptions) package,
which GHC has included since version 8.10, supersedes this package, and new
code should use it. Its `MonadThrow`, `MonadCatch`, and `MonadMask` classes
replace `MonadException` and `MonadAsyncException`, and its `CatchT` replaces
`ExceptionT`. This package remains maintained for existing users.
