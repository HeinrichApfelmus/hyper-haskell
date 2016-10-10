/* *************************************************************
    Managing Haskell interpreter instances
************************************************************* */

// The present module is used by both the main process and the
// browser window processs.
// For this to work, we can't load all imported modules right away.
// Instead, we define them first, and load them later.
let app    = {}
let ipc    = {}
let remote = {}

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
  ipc = electron.ipcMain
  app = electron.app
  
  const appdir = require('path').dirname(app.getAppPath())

  // function that spawns a new interpreter process
  ipc.on('interpreter-start', (event, id, cwd, packageTool, packagePath) => {
    // kill previous interpreter if necessary
    exports.main.kill(id)

    // assign a new port number
    let port = ports
    ports += 1

    // setup environment
    let env      = {}
    env['HOME']  = app.getPath('home') // necessary for finding package database
    // FIXME: Where to we get the path from as a standalone application?
    env['PATH']  = process.env['PATH'] // pick up path from external environment if possible
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
      cmd  = env['HOME'] + '/.cabal/bin/hyper-haskell-server'
    }
    
    // spawn process
    let ghc = child_process.spawn(cmd, args, {
      cwd  : cwd ? cwd : env['HOME'],
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
      error = code.toString()
      console.log('Interpreter stopped (Exit code ' + error + ')')
    })
    const whenReady = () => {
      if (!error) {
        ghcs[id] = ghc
        // communicate port number to worksheet process
        event.sender.send('interpreter-started', port)
      } else {
        // we could not start the interpreter process
        event.sender.send('interpreter-down', error)
      }
    }
    // FIXME: more accurate indication that the interpreter process is ready
    setTimeout(whenReady, 2400)
  })
}

// Kill interpreter associated to a particular window ID
exports.main.kill = (id) => {
  // FIXME: only send signal if child process is still running!
  // otherwise another process with that PID could get the signal.
  if (ghcs[id]) { ghcs[id].kill('SIGTERM') }
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
  ipc            = electron.ipcRenderer
  remote         = electron.remote
  
  ipc.on('interpreter-started', (event, port) => {
    down   = null
    myPort = port
    exports.send('interpreter-ready')
  })
  ipc.on('interpreter-down', (event, err) => {
    down   = err
    myPort = 0
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

// ( list of imported modules, list of files to load, continuation )
const loadImports = (imports, newfiles, cont) => {
  const withLoadedFiles = (importModules) => {
    // load source code files only if absolutely necessary,
    // because that resets the interpreter state
    if (files !== newfiles) {
      ajax({
        method : 'POST',
        url    : 'http://localhost:' + myPort.toString() + '/loadFiles',
        data   : { query: newfiles.join(',') },
        dataType: 'json',
      }, (result) => {
        if (result.status === 'ok') {
          files = newfiles
          importModules()
        } else { cont(result) }
      })
    } else { importModules() }
  }
  
  withLoadedFiles( () => {
    ajax({
      method : 'POST',
      url    : 'http://localhost:' + myPort.toString() + '/setImports',
      data   : { query: imports.join(',') },
      dataType: 'json',
    }, cont)
  })
}

// Load modules, perhaps spawn a new process
exports.renderer.loadImports = (config, cont) => {
  const doImports = () => { loadImports(config.imports, config.files, cont) }
  
  if (   config.packagePath !== packagePath
      || config.packageTool !== packageTool
      || config.cwd         !== cwd) {
    // we have to spawn a new interpreter process with the right package tool and path
    packagePath = config.packagePath
    packageTool = config.packageTool
    cwd         = config.cwd
    // load imports when interpreter is done loading
    exports.on('interpreter-ready', () => { doImports() })
    ipc.send('interpreter-start', remote.getCurrentWindow().id,
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
