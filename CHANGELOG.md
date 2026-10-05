# Changelog

## Unreleased

- Lift `finally` through `IdentityT`, `ReaderT`, and `ListT`, which now
  delegate to the underlying monad's `finally`. Cleanup runs when the
  underlying monad short-circuits if the underlying `finally` runs it then, as
  the `ExceptT` and `MaybeT` instances do. Cleanup runs once per `ListT`
  computation instead of once per result, and it uses the underlying monad's
  masking.
- `finally` in `IdentityT`, `ReaderT`, and `ListT` now discards a short circuit
  in the cleanup itself when the underlying `finally` does. A `ListT` cleanup
  that produces `[]` or several results no longer changes the results, and the
  `ExceptT` and `MaybeT` instances discard a cleanup's `Left` or `Nothing`. For
  example, ``runExceptT (runReaderT (return 1 `finally` lift (throwE e)) r)``
  now gives `Right 1` instead of `Left e`. An exception raised by the action is
  no longer replaced by a short circuit in the cleanup.
- Document the existing `finally` behavior of the state, writer, and RWS
  transformers, which is unchanged. Cleanup after success keeps its effects on
  state and output. Cleanup does not run when the underlying monad
  short-circuits without raising an exception. The `exceptions` package
  provides `generalBracket` for stacks that need that cleanup.
- Drop GHC 7.x and add GHC 9.8, 9.10, 9.12, and 9.14 to the tested compilers.
  Test with the latest patch release of each GHC series from 8.0 through 9.14.
- Require `base >= 4.9` and `transformers >= 0.5`, matching the supported GHC
  8.0+ range. Drop the `fail` and `transformers-compat` dependencies and the
  compatibility code for older compilers.
- Use HTTPS for the source repository and include the README and changelog in
  source distributions.
