## Changelog for HyperHaskell


### HyperHaskell app bundle

**v2.1.0** — Snapshot release.

* Upgrade file format to version `0.2.1.0`.
* Add input field for setting the language extensions that are used for interpreting code.

**v2.0.0** — Snapshot release.

* Upgrade file format to version `0.2.0.0`. Conversion of old files happens *automatically* when loading them in the application.
* Support different cell types:
    1. 'code' cells -- for source code to be evaluated
    2. 'text' cells -- plain text format
* Compatibility with the [Nix package manager][nix] (by Rodney Lorrimar).

  [nix]: https://nixos.org/nix/

**v1.0.0**

* Initial release.

### `hyper-haskell-server` package

**v0.2.1.0**

* Add interpretation of `let` statements and generator statements with patterns, `pat <- stmt`. The same syntax as GHC is accepted.
* Add `setExtensions` method to set the language extensions that are used for interpreting code.
* Compatibility with GHC 8.4.

**v0.2.0.0**

* Add interpretation of statements of the form `name <- stmt`. This allows us to bind values and functions to names. Here, `stmt` is an IO action with a result.

**v0.1.0.2** — Bump dependencies for compatibility with GHC 8.2

**v0.1.0.1** — Bump dependencies.

**v0.1.0.0** — Initial release.

### `hyper`, `hyper-extra` packages

**v0.1.0.2** — Bump dependencies for compatibility with GHC 8.2

**v0.1.0.1** — Bump dependencies.

**v0.1.0.0** — Initial release.
