TODO list
=========

Immediate
---------

* Fix `setRepresentedFilename` on non-macOS platforms.

* Implement text and heading cell types.

* Level β:

    * Implement `runStmt` function to bind values while doing IO actions.
      Cannot be implemented by evaluating expressions.

    * Implement a way to evaluate cells concurrently.
      We still want to be able to bind variables sequentially.
      Using `forkIO` explicitely would do, but then we can't stop it again.

      Maybe a "suspended result"?

Later
-----

* Integrate a viewer for Haddock documentation

* Integrate a way to query type signatures

* Maybe implement editor for `.hs` files? The `FiraCode` font is quite beautiful.

* Switch programming of user interface to some other language / approach.
  * sodium.js ?
  * threepenny-gui ?

* FIXME: Maybe do *not* move the cursor after evaluating a cell?

* Level γ:

    * Integrate some form of a JavaScript FFI
    * Export Haskell functions to JavaScript
    * Suspended computations -- display infinite list

Much later
----------

* Bundle the `hyper-haskell-server` executable and a package database with the front-end,
  so that the user has a default Haskell interpreter to work with.
  The user is still free to choose another database if he likes to.
  
  Implementing this would require [relocatable Haskell packages][relocatable].
  Unfortunately, they are not readily supported yet,
  and I don't want to spend much time with it.
      
  [relocatable]: https://github.com/haskell/cabal/issues/462

