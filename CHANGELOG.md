## Changelog for HyperHaskell


### HyperHaskell app bundle

**v2.4.0** — Snapshot release.

* Works with `hyper-haskell-server` version 0.2.4.0.
* Support `.ghc.environment` files.
* Update Electron to version 38.2.2.

**v2.3.0** — Snapshot release.

* Add `~/.ghcup` directory to PATH by default.
* Add `View` menu and zoom functionality.
* Update Electron to version 10.1.5.

**v2.2.0** — Snapshot release.

* Implement "Search path" field properly.
* Add "Open Recent…" menu item on macOS.

* Update Electron to version 3.1.8.
* Support for packaging binaries on Windows (by Nicholas Silvestrin).

* Fix "Save As…" menu item to work correctly again.
* Fix bug where no new input cell would be created when evaluating the last one.

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

**v0.2.4.0**

* Works with HyperHaskell app version v2.4.0.
* Less time to wait after interpreter starts up successfully.
* Report slightly more errors.
* Bump depenencies to allow `scotty-0.22`.

**v0.2.3.1** — Bump dependencies

**v0.2.3.0**

* Compatibility with GHC 8.8.
* Remove support for `hyper == 0.1.*`.

**v0.2.2.0**

* Add `:type` command for querying type signatures.
* Add support for qualified imports with optional aliases. For example, lines in the "Module imports" field can now have the form `import qualified Data.Map as Map`. [#27][]

  [#27]: https://github.com/HeinrichApfelmus/hyper-haskell/issues/27

**v0.2.1.0**

* Add interpretation of `let` statements and generator statements with patterns, `pat <- stmt`. The same syntax as GHC is accepted.
* Add `setExtensions` method to set the language extensions that are used for interpreting code.
* Compatibility with GHC 8.4.

**v0.2.0.0**

* Add interpretation of statements of the form `name <- stmt`. This allows us to bind values and functions to names. Here, `stmt` is an IO action with a result.

**v0.1.0.2** — Bump dependencies for compatibility with GHC 8.2

**v0.1.0.1** — Bump dependencies.

**v0.1.0.0** — Initial release.

### `hyper-extra` package

**v0.2.0.2** — Bump dependencies, modernize `.cabal` file.

**v0.2.0.1** — Bump dependencies

**v0.2.0.0**

* Add preliminary support for the `svg-builder` package.
* Compatibility with GHC 8.8

**v0.1.1.0**

* Add preliminary support for the `QuickCheck` package.

**v0.1.0.3** — Bump dependencies for compatibility with GHC 8.4

**v0.1.0.2** — Bump dependencies for compatibility with GHC 8.2

**v0.1.0.1** — Bump dependencies.

**v0.1.0.0** — Initial release.

### `hyper`  package

**v0.2.1.2** — Bump dependencies, modernize `.cabal` file.

**v0.2.1.1** — Bump dependencies

**v0.2.1.0**

* Add `addFinalizerSession` function that allows running an IO action when the worksheet is reloaded.
* Compatibility with GHC 8.8.

**v0.2.0.0**

* Remove `displayIO` class.
* Make `string` wrap its contents in a `<div>` element.

**v0.1.0.3** — Bump dependencies for compatibility with GHC 8.4

**v0.1.0.2** — Bump dependencies for compatibility with GHC 8.2

**v0.1.0.1** — Bump dependencies.

**v0.1.0.0** — Initial release.
