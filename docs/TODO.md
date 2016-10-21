TODO list
=========

Immediate
---------

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
  * GHCJS + reactive-banana ?

* FIXME: Maybe do *not* move the cursor after evaluating a cell?

* Level γ:

    * Integrate some form of a JavaScript FFI
    * Export Haskell functions to JavaScript
    * Suspended computations -- display infinite list

Much later
----------

* Bundle an interpreter back-end with the front-end application. #10

