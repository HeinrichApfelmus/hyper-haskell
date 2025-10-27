/* *************************************************************
    Managing Haskell interpreter instances
    Main process
************************************************************* */

/* *************************************************************
    NOTE [InterpreterLifetime]
************************************************************* */
/*
This note describes the communication flow for starting
interpreter processes.

The browser window does not care whether an interpreter
process has been started or not. The window simply calls
the function `interpreter.loadImports()` to bring
imports into scope. This function has to take care of
starting or stopping interpreter instances accordingly.

Interpreter processes are managed by the main process.
The `interpreter.loadImports()` function sends
an IPC event 'startInterpreter' to the main process
in order to start an interpreter process.
Once the interpreter has been started, the main process
sends an IPC event 'doneInterpreterStart' to the
browser window.

As starting the interpreter happens asynchronously,
two classes of errors may happen:

  1) The browser window is closed before the
     'doneInterpreterStart' event could be sent.
     In this case, the main process kills the interpreter again.

  2) The browser window attempts to evaluate
     code before the interpreter has started.

*/

const child_process   = require('child_process')
const {app , BrowserWindow , ipcMain} = require('electron')
const lib = {
  fs: require('node:fs'),
  path: require('node:path'),
  process: require('process')
}

let ghcs  = []     // interpreter processes for different window IDs
// FIXME: Obtain temporary ports more properly
let ports = 14200  // keep track of assigned port numbers

let stackPath = ''    // path to the stack utility
exports.setPaths = (p) => {
  stackPath = p
}

// Initialize the interpreter in the main process
exports.init = () => {
  const isWindows = lib.process.platform === "win32";
  const appdir    = lib.path.dirname(app.getAppPath())

  // function that spawns a new interpreter process
  ipcMain.on('startInterpreter', (event, cwd, packageTool, packagePath) => {
    // kill previous interpreter if necessary
    const id = BrowserWindow.fromWebContents(event.sender).id
    exports.kill(id)

    // assign a new port number
    let port = ports
    ports += 1

    // setup environment
    let env      = {}
    env['HOME']  = app.getPath('home') // necessary for finding package database
    // FIXME: Where do we get the path from as a standalone application?
    // pick up path from external environment if possible
    env['PATH']  = lib.process.env['PATH'] + ':/usr/bin:/usr/local/bin:' + env['HOME'] + '/.ghcup/bin'
    env['PORT']  = port.toString()
    let cmd      = ''
    let args     = []

    if (packageTool == 'stack') {
      if (stackPath) {
        cmd = stackPath
      } else {
        cmd = 'stack'
      }
      args = ['--stack-yaml=' + packagePath, 'exec', '--', 'hyper-haskell-server']
    } else if (packageTool == 'cabal') {
      if (isWindows) {
        cmd  = env['HOME'] + '\\AppData\\Roaming\\cabal\\bin\\hyper-haskell-server'
      } else {
        // `cabal` >= 3.10 installs binaries in `~/.local/bin` by default.
        // Old versions of `cabal` have installed in `.cabal/bin`.
        // We assume the default paths and do *not* check the cabal config file.
        cmdNew = env['HOME'] + '/.local/bin/hyper-haskell-server'
        cmdOld = env['HOME'] + '/.cabal/bin/hyper-haskell-server'
        if (lib.fs.existsSync(cmdOld)) {
          cmd = cmdOld
        } else {
          cmd = cmdNew
        }
        if (packagePath) {
          cmd = cwd + '/' + packagePath + '/hyper-haskell-server'
        }
      }
    } else if (packageTool == 'cabal.project') {
      cmd = 'cabal'
      // FIXME?: Add cabal project dir that is relative to the current worksheet
      // args = ['exec', '--project-dir', lib.path.dirname(packagePath), 'hyper-haskell-server']
      // NOTE: We use 'exec' instead of 'run' because we expect
      // the developer to have build the executable 'hyper-haskell-server'.
      // Using 'run' may change .ghc.environment files.
      args = ['exec', 'hyper-haskell-server'] 
    } else if (packageTool == 'nix') {
      cmd = 'hyper-haskell-server'
      env = Object.assign({}, lib.process.env, { PORT: port.toString() })
    }

    // spawn process
    let spawning = true
    let pipeFD   = 3 // file descriptor for pipe
    let ghc = child_process.spawn(cmd, args.concat([pipeFD.toString()]), {
      cwd  : cwd,
      env  : env,
      stdio    : ['inherit', 'inherit', 'inherit', 'pipe'],
      encoding : 'utf8',
    })
  
    const doReady = (error) => {
      spawning = false
      if (!error) { ghcs[id] = ghc } else { port = 0 }
      // Indicate that the interpreter is ready only when the window is still alive
      // See Note [InterpreterLifetime]
      if (event.sender.isDestroyed()) {
        exports.main.kill(id)
      } else {
        event.sender.send('done-interpreter-start', port, error)
      }
    }

    // Interpreter reports that it is ready
    ghc.stdio[pipeFD].on('data', (data) => {
      if (data.toString() === "ready") {
        doReady(null)
      } else {
        doReady('Malformed interpreter response: ' + data.toString())
      }
    })
    ghc.on('error', (err) => {
      // set an error code when the interpreter process could not be started
      console.log('Interpreter not running: ' + err.message)
      doReady(err.message)
    })
    ghc.on('exit', (code, signal) => {
      // FIXME: Report a more useful error here.
      // Unfortuntaley, we can't read `stderr` once the process is dead.
      // Perhaps cache it in some way?
      // Idea: The interpreter executable can print the error to
      // the pipeFD descriptor, as opposed to stdout
      error = code ? code.toString() : '0';
      console.log('Interpreter stopped (Exit code ' + error + ')')
      if (spawning) { doReady(error) }
      // FIXME: Tell browser window something appropriate when the interpreter dies unexpectedly
    })
    setTimeout( () => {
      if (spawning) { doReady('Interpreter timeout') }
    }, 10*1000)
  })
}

// Kill interpreter associated to a particular window ID
exports.kill = (id) => {
  // FIXME: only send signal if child process is still running!
  // otherwise another process with that PID could get the signal.
  if (ghcs[id]) {
    ghcs[id].kill('SIGTERM')
    ghcs[id] = null
  }
}
