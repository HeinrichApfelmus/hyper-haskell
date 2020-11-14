/* *************************************************************
    interpreter.js
    Managing Haskell interpreter instances
************************************************************* */

/* NOTE [DualImports]

  The present module collects all functions relating
  to managing interpreters and their lifetime in one place.

  Thus, this module is imported by both the main process and the browser window process.
  The main process only uses functions from `exports.main.*`,
  whereas the browser window process will only use `exports.renderer.*`.
  Communciation is done via IPC, it is not possible to share global state,
  as this module is loaded twice in different processes.

  For this setup to work, we cannot load imported modules right away,
  we load them in  main.init()  and  renderer.init() .
  For convenience, we define a few modules names that are global,
  and will be loaded later.
*/
let app    = {}
let ipc    = {}
let remote = {}

/* *************************************************************
    NOTE [InterpreterLifetime]
************************************************************* */
/*
This note describes the communication flow for starting
interpreter processes.

The browser window does not care whether an interpreter
process has been started or not. The window simply calls
the function `exports.renderer.loadImports()` to bring
imports into scope. This function has to take care of
starting or stopping interpreter instances accordingly.

Interpreter processes are managed by the main process.
The `exports.renderer.loadImports()` function sends
an IPC event 'do-interpreter-start' to the main process
in order to start an interpreter process.
Once the interpreter has been started, the main process
sends an IPC event 'done-interpreter-start' to the
browser window.

As starting the interpreter happens asynchronously,
two classes of errors may happen:

  1) The browser window is closed before the
     'done-interpreter-start' event could be sent.
     In this case, the main process kills the interpreter again.

  2) The browser window attempts to evaluate
     code before the interpreter has started.

*/

/* *************************************************************
    Main process
************************************************************* */
exports.main = {}

let ghcs  = []     // interpreter processes for different window IDs
// FIXME: Obtain temporary ports more properly
let ports = 14200  // keep track of assigned port numbers

let stackPath = ''    // path to the stack utility
exports.main.setPaths = (p) => {
  stackPath = p
}

// Initialize the interpreter in the main process
exports.main.init = () => {
  // require modules specific to main process
  const child_process = require('child_process')
  const process       = require('process')
  const electron      = require('electron')
  ipc = electron.ipcMain  // see Note [DualImports]
  app = electron.app      // see Note [DualImports]

  const isWindows = process.platform === "win32";
  const appdir    = require('path').dirname(app.getAppPath())

  // function that spawns a new interpreter process
  ipc.on('do-interpreter-start', (event, id, cwd, packageTool, packagePath) => {
    // kill previous interpreter if necessary
    exports.main.kill(id)

    // assign a new port number
    let port = ports
    ports += 1

    // setup environment
    let env      = {}
    env['HOME']  = app.getPath('home') // necessary for finding package database
    // FIXME: Where to we get the path from as a standalone application?
    // pick up path from external environment if possible
    env['PATH']  = process.env['PATH'] + ':/usr/bin:/usr/local/bin:' + env['HOME'] + '/.ghcup/bin'
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
        cmd  = env['HOME'] + '/.cabal/bin/hyper-haskell-server'
      }
    } else if (packageTool == 'nix') {
      cmd = 'hyper-haskell-server'
      env = Object.assign({}, process.env, { PORT: port.toString() })
    }

    // spawn process
    let ghc = child_process.spawn(cmd, args, {
      cwd  : cwd,
      env  : env,
      stdio: 'inherit',
      encoding : 'utf8',
    })
    let error = null
    ghc.on('error', (err) => {
      // set an error code when the interpreter process could not be started
      error = err.message
      console.log('Interpreter not running: ' + error)
    })
    ghc.on('exit', (code, signal) => {
      // FIXME: Report a more useful error here.
      // Unfortunatley, we can't read `stderr` once the process is dead.
      // Perhaps cache it in some way?
      error = code ? code.toString() : '0';
      console.log('Interpreter stopped (Exit code ' + error + ')')
    })
    const whenReady = () => {
      if (!error) {
          ghcs[id] = ghc
      } else {
          port = 0
      }
      // Indicate that the interpreter is ready only when the window is still alive
      // See Note [InterpreterLifetime]
      if (event.sender.isDestroyed()) {
        exports.main.kill(id)
      } else {
        event.sender.send('done-interpreter-start', port, error)
      }
    }
    // FIXME: more accurate indication that the interpreter process is ready
    // FIXME: The window may try to connect to the interpreter when it has not started yet.
    setTimeout(whenReady, 2400)
  })
}

// Kill interpreter associated to a particular window ID
exports.main.kill = (id) => {
  // FIXME: only send signal if child process is still running!
  // otherwise another process with that PID could get the signal.
  if (ghcs[id]) {
    ghcs[id].kill('SIGTERM')
    ghcs[id] = null
  }
}

/* *************************************************************
    Renderer process
************************************************************* */
exports.renderer = {}

let myPort      = 0       // port number of the connected interpreter
let down        = 'not started' // error message why the interpreter may be down
let packagePath = null    // last used path for the interpreter executable
let packageTool = null    // last used package tool
let cwd         = null    // last used current working directory
let files       = []      // list of previously loaded files

// Initialize interpreter-related code for a given window
exports.renderer.init = (window) => {
  // require modules specific to main process
  const electron = require('electron')
  ipc            = electron.ipcRenderer // see Note [DualImports]
  remote         = electron.remote      // see Note [DualImports]

  ipc.on('done-interpreter-start', (event, port, err) => {
    down   = err
    myPort = port
    exports.send('interpreter-ready')
  })
}

// send an ajax request to the interpreter process
const ajax = (config, cont) => {
  if (!down) {
    // interpreter is up and running
    if (cont) { $.ajax(config).done(cont) } else { $.ajax(config) }
  } else {
    // interpreter is down, return reason.
    // + JSON.stringify(require('process').env)
    cont({ status: 'error', errors: ['Interpreter not running (Error: ' + down + ')'] })
  }
}

// Evaluate an expression
//   cont = ({ status : 'ok'/'error' , value:... }) => { ... }
exports.renderer.eval = (expr, cont) => {
  ajax({
    method : 'POST',
    url    : 'http://localhost:' + myPort.toString() + '/eval',
    data   : { query: expr },
    dataType: 'json',
  }, cont)
}

// Cancel evaluation
exports.renderer.cancel = () => {
  ajax({
    method : 'POST',
    url    : 'http://localhost:' + myPort.toString() + '/cancel',
  })
}

// config.imports.   = list of module imports
// config.extensions = list of extension to use
// config.files      = new list of files to load
// config.searchPath = search path for finding these files
// config.cwd        = directory that the notebook is contained in
// cont              = continuation in case of error or success
const loadImports = (config, cont) => {
  const withLoadedFiles = (importModules) => {
    // load source code files only if absolutely necessary,
    // because that resets the interpreter state
    if (files !== config.files) {
      ajax({
        method : 'POST',
        url    : 'http://localhost:' + myPort.toString() + '/setSearchPath',
        data   : { query: config.searchPath, dir: config.cwd },
        dataType: 'json',
      }, (result) => {
        if (result.status === 'ok') {
          ajax({
            method : 'POST',
            url    : 'http://localhost:' + myPort.toString() + '/loadFiles',
            data   : { query: config.files.join(',') },
            dataType: 'json',
          }, (result) => {
              if (result.status === 'ok') {
                files = config.files
                importModules()
              } else { cont(result) }
          })
        } else { cont(result) }
      })
    } else { importModules() }
  }

  withLoadedFiles( () => {
    ajax({
      method : 'POST',
      url    : 'http://localhost:' + myPort.toString() + '/setImports',
      data   : { query: config.imports.join(',') },
      dataType: 'json',
    }, (result) => {
		if (result.status === 'ok') {
			ajax({
				method : 'POST',
				url    : 'http://localhost:' + myPort.toString() + '/setExtensions',
				data   : { query: config.extensions.join(',') },
				dataType: 'json',
			}, cont)
		} else { cont(result) }
	})
  })
}

// Load modules, perhaps spawn a new process
exports.renderer.loadImports = (config, cont) => {
  const doImports = () => { loadImports(config, cont) }

  if (   config.packagePath !== packagePath
      || config.packageTool !== packageTool
      || config.cwd         !== cwd) {
    // we have to spawn a new interpreter process with the right package tool and path
    packagePath = config.packagePath
    packageTool = config.packageTool
    cwd         = config.cwd
    // load imports when interpreter is done loading
    exports.on('interpreter-ready', () => { doImports() })
    ipc.send('do-interpreter-start', remote.getCurrentWindow().id,
      cwd, packageTool, packagePath)
  } else {
    // the interpreter is running and we simply reload imports
    doImports()
  }
}


/* *************************************************************
  Miniature message passing framework
************************************************************* */
let handlers = []

exports.on   = (name, fun) => { handlers[name] = fun }
// Call an event handler with a variable number of arguments
exports.send = (name) => {
  if (handlers[name]) {
    const args = Array.prototype.slice.call(arguments, 1)
    handlers[name].apply(null, args)
  }
}
