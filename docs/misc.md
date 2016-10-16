Misc
=====

This documentation collects useful miscellanea.

Testing POST requests
---------------------

The `curl` utility is able to send POST requests and receive a response. Example:

    $ curl --data "query=Prelude" http://localhost:8024/setImports
    {"status":"ok"}
    $ curl --data "query=3*4::Int" http://localhost:8024/eval
    {"status":"ok","value":"12"}

Note that `curl` will not, by default, add a newline to the end of the received request; this may screw up the terminal. However, the option `-w '\n'` will add said newline. To make it a default, use the `~/.curlrc` file [[StackOverflow]][12849584].

    echo '-w "\n"' >> ~/.curlrc

  [12849584]: http://stackoverflow.com/questions/12849584/automatically-add-newline-at-end-of-curl-response-body


How does GHCi evaluate expressions on the prompt?
-------------------------------------------------

In particular, the question is how GHCi distinguishes between expressions of `IO` type, which are executed, and expressions of non-IO type, which are wrapped in a call to `show`.

Let's have a look at the [GHC source code](https://github.com/ghc/ghc). Following the execution path, we find the following functions:

* [GHCi.UI.runStmt](https://github.com/ghc/ghc/blob/ghc-8.0/ghc/GHCi/UI.hs#L941)

    "Entry point to execute some haskell code from user"

    The prompt analyzed and falls into 3 cases: "import", statement, declaration.

* [GHCi.UI.Monad.runStmt](https://github.com/ghc/ghc/blob/ghc-8.0/ghc/GHCi/UI/Monad.hs#L310)

    "Run a single Haskell expression"

    This function essentially just calls `GHC.execStmt`.

* [GHC.execStmt](https://github.com/ghc/ghc/blob/master/compiler/main/InteractiveEval.hs#L164)

    "Run a statement in the current interactive context."

    This function is part of the public GHC API.

Unfortunately, this is as far as I got. I don't quite understand what a `HValue` is.

Fortunately, a Google search reveals that the relevant code is actually contained in the type checker.

* [compiler/typecheck/TcRnDriver.hs](https://github.com/ghc/ghc/blob/ghc-8.0/compiler/typecheck/TcRnDriver.hs#L1737)

    Section "Typechecking Statements in GHCi".

    > By 'lift' and 'environment we mean that the code is changed to
    execute properly in an IO monad. See Note [Interactively-bound Ids
    in GHCi] in HscTypes for more details. We do this lifting by trying
    different ways ('plans') of lifting the code into the IO monad and
    type checking each plan until one succeeds.

    Apparently,

    > The plans are:
    >
    > A. [it <- e; print it]     but not if it::()
    >
    > B. [it <- e]
    >
    > C. [let it = e; print it]

In other words, GHC tries to typecheck the expression at the prompt in different contexts, and uses the first plan that works.


How to create an .icns file on OS X?
------------------------------------

Application icons on OS X are stored in `.icns` files. There is a command line utility called `iconutil` that is supposed to be able to create such files, but unfortunately, it only creates garbled icons for me. No idea why. Online converters seem to work fine.

How to remove color profiles from images on OS X?
-------------------------------------------------

    sips -d profile --deleteColorManagementProperties *filename*


Haskell path information
------------------------

**Cabal**

The configuration file is in 

    ~/.cabal/config

It contains several fields that are relevant:

    extra-prog-path: /Users/apfelmus/.cabal/bin


**Stack**

Once we know where a particular `stack.yml` file is, we can use

    stack --stack-yaml=foo/stack.yaml

to use the stack utility relative to this location. We can query various paths, in particular `GHC_PACKAGE_PATH`:

    stack path --ghc-package-path

But to execute the interpreter server, including the right `GHC_PACKAGE_PATH`, it is actually simpler to just call

    stack exec hyper-haskell-server             # execute binary

The path of the interpreter executable can be obtained from

    stack exec -- which hyper-haskell-server    # retrieve full path of the binary 

For my setup, a minimal environment is given by

    env -i \
        PATH=/usr/bin:/Applications/ghc-7.10.3.app/Contents/bin \
        HOME=/Users/hgz \
        stack exec -- hyper-haskell-server


Node.js -- External Processes
-----------------------------

We can run external processes and read their `stdout`. Example:

    let result = process.spawnSync(ghcbindir + '/stack',
        ['exec', 'printenv', '--', 'GHC_PACKAGE_PATH'], {
        cwd : dir,
        encoding : 'utf8',
        env : newEnv(),
      }).stdout.trim()

Note that most UNIX utilities will traditionally emit a newline at the end of the output, and we use `trim()` to get rid of it.