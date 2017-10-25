TODO list
=========

Immediate
---------

* Level β:

    * Implement a way to evaluate cells concurrently.
      We still want to be able to bind variables sequentially.
      Using `forkIO` explicitly would do, but then we can't stop it again.

      Maybe a "suspended result"?
      
      -> I think that this is not necessary if we have a version of 'forkIO'
      that can be killed again with the click of a button?

    * Make the process of hooking threepenny-gui directly into the interpreter more straightforward.
    
      Maybe a button or checkbox where we connect or disconnect the whole thing at once?

      Ah, that doesn't work, because we can't return a value while *also* displaying some kind of 'Graphic' result.
    
    * Find a way to display 'Graphic' while returning a value. 
        
      Printing graphics as a side effect? Do we really want this? Probably yes?

Small
-----

* Better scrollability of the 'Status:' field in case of compilation errors.

* Save As… does not seem to work correctly!


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

