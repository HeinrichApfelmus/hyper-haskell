## Changelog for HyperHaskell


### HyperHaskell app bundle

**v2.0.0** — Snapshot release.

* File format upgraded to version `0.2.0.0`. Conversion of old files happens *automatically* when loading them in the application.
* Support different cell types:
    1. 'code' cells -- for source code to be evaluated
    2. 'text' cells -- plain text format
* Compatibility with the [Nix package manager][nix] (by Rodney Lorrimar).

  [nix]: https://nixos.org/nix/

**v1.0.0**

* Initial release.

### `hyper`, `hyper-extra`, `hyper-haskell-server` packages

**v0.1.0.2** — Bump dependencies for compatibility with GHC 8.2

**v0.1.0.1** — Bump dependencies.

**v0.1.0.0** — Initial release.
