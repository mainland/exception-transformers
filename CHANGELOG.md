# Changelog

## Unreleased

- Drop GHC 7.x and add GHC 9.8, 9.10, 9.12, and 9.14 to the tested compilers.
  Test with the latest patch release of each GHC series from 8.0 through 9.14.
- Require `base >= 4.9` and `transformers >= 0.5`, matching the supported GHC
  8.0+ range. Drop the `fail` and `transformers-compat` dependencies and the
  compatibility code for older compilers.
- Use HTTPS for the source repository and include the README and changelog in
  source distributions.
