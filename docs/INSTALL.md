**work in progress**

<del>
# Installation

A HyperHaskell installation consists of two parts:

1. The graphical front-end.

    Download the application binary for your platform: OS X, (Linux), (Windows)

2. The interpreter back-end.

    HyperHaskell requires several Haskell packages to work properly.
    In order to not restrict you to outdated package versions,
    the front-end asks you choose your own package database.
    However, this means that you have to install the interpreter back-end separately.

    If you use [Cabal][]:
    
    * Install the back-end by executing the command

            cabal update && cabal install hyper-haskell

    * The front-end will, by default, look for this option first.
    You only need to start the application and should be ready to go.

    If you use [Stack][]:
    
    * Make a new `stack.yml` file somewhere
    * Use `stack install hyper-haskell` to install the back-end.
    * Start the front-end and point it to the `stack.yml` file in the settings.

  [cabal]: https://www.haskell.org/cabal/
  [stack]: https://www.haskellstack.org
</del>
