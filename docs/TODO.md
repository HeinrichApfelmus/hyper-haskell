TODO list
=========

Immediate
---------

* FIXME: Maybe do *not* move the cursor after evaluating a cell?

* Add `make osx` target that creates a OS X application bundle with everything in it
    * Idea: Actually, we *can* bundle the executable and a package database with
      the front-end. It's just that the user is free to choose another database if he likes.

Later
-----

* Add a preference for setting the `PATH` variable.
  Perhaps even read it from `~/.profile` if possible?

* Integrate a viewer for Haddock documentation

* Integrate a way to query type signatures

* Implement a `loadFile` operation
  * Maybe even implement editor for files?

* Switch programming of user interface to some other language / approach.
  * sodium.js ?
  * threepenny-gui ?

* For IO actions:
  Set working directory to be the directory where the worksheet file was located

* Implement `runStmt` function to bind values while doing IO actions.
  Cannot be implemented by evaluating expressions.

Much later
----------

* Level β
