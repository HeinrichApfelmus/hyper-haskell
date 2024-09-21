/* *************************************************************
    Interacting with a Haskell interpreter instance
    Renderer
************************************************************* */

/* Requires
window.electron.startInterpreter
window.electron.onDoneInterpreterStart
*/
window.newInterpreter = () => {
    var that = {}

    let myPort = 0           // port number of the connected interpreter
    let down = 'not started' // error message why the interpreter may be down
    let packagePath = null   // last used path for the interpreter executable
    let packageTool = null   // last used package tool
    let cwd = null // last used current working directory
    let files = [] // list of previously loaded files

    // send an ajax request to the interpreter process
    const ajax = (config, cont) => {
        if (!down) {
            // interpreter is up and running
            if (cont) { $.ajax(config).done(cont) } else { $.ajax(config) }
        } else {
            // interpreter is down, return reason.
            // + JSON.stringify(require('process').env)
            cont({
                status: 'error',
                errors: ['Interpreter not running (Error: ' + down + ')']
            })
        }
    }

    // Evaluate an expression
    //   cont = ({ status : 'ok'/'error' , value:... }) => { ... }
    that.eval = (expr, cont) => {
        console.log('interpreter.eval')
        ajax({
            method : 'POST',
            url    : 'http://localhost:' + myPort.toString() + '/eval',
            data   : { query: expr },
            dataType: 'json'
        }, cont)
    }

    // Cancel evaluation
    that.cancel = () => {
        console.log('interpreter.cancel')
        ajax({
            method : 'POST',
            url : 'http://localhost:' + myPort.toString() + '/cancel'
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
                dataType: 'json'
            }, (result) => {
                if (result.status === 'ok') {
                ajax({
                    method : 'POST',
                    url    : 'http://localhost:' + myPort.toString() + '/loadFiles',
                    data   : { query: config.files.join(',') },
                    dataType: 'json'
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
                dataType: 'json'
            }, (result) => {
                if (result.status === 'ok') {
                    ajax({
                        method : 'POST',
                        url    : 'http://localhost:' + myPort.toString() + '/setExtensions',
                        data   : { query: config.extensions.join(',') },
                        dataType: 'json'
                    }, cont)
                } else { cont(result) }
            })
        })
    }

    // Load modules, perhaps spawn a new process
    that.loadImports = (config, cont) => {
        console.log('interpreter.loadImports')
        const doImports = () => {
            console.log('interpreter.doImports')
            loadImports(config, cont)
        }
        if (   config.packagePath !== packagePath
            || config.packageTool !== packageTool
            || config.cwd         !== cwd) {
            // we have to spawn a new interpreter process with the right package tool and path
            packagePath = config.packagePath
            packageTool = config.packageTool
            cwd         = config.cwd
            // load imports when interpreter is done loading
            that.onInterpreterStarted(doImports)
            window.electron.startInterpreter(cwd, packageTool, packagePath)
        } else {
            // the interpreter is running and we simply reload imports
            doImports()
        }
    }

    // Register a callback for when the the interpreter has started.
    that.callbackInterpreterStarted = () => {}
    that.onInterpreterStarted = (callback) => {
        that.callbackInterpreterStarted = callback
    }
    that.init = (window) => {
        console.log('interpreter.init')
        window.electron.onDoneInterpreterStart( (port, err) => {
            down = err
            myPort = port
            that.callbackInterpreterStarted()
        })
    }

    return that
}
