/* *************************************************************
    Main program
************************************************************* */
let TESTING = require('process').env['TESTING'] ? true : false

const electron    = require('electron')
const { app, dialog, BrowserWindow, Menu, MenuItem } = electron
const ipc         = electron.ipcMain
const fs          = require('fs')
const interpreter = require('./interpreter.js')

const appdir      = require('path').normalize(__dirname + "/..")
const resolvePath = require('path').resolve

/* ****************************************************************
  Initialization
**************************************************************** */

app.on('ready', () => {
  // initialize interpreter
  interpreter.main.init()
  // setup menu bar
  Menu.setApplicationMenu(Menu.buildFromTemplate(template))
  
  // load preferences and apply them
  applyPreferences(loadPreferences())
  ipc.on('save-preferences', (event, prefs) => {
    applyPreferences(prefs)
    savePreferences(prefs)
  })

  // [setDocumentEdited]
  // We use an event handler to call "setDocumentEdited" on a window
  ipc.on('setDocumentEdited', (event, value) => {
    BrowserWindow.fromWebContents(event.sender).setDocumentEdited(value)
  })
  
  // restore 'open-file' to open files directly, instead of queuing them
  app.removeListener('open-file', addPathToOpen)
  app.on('open-file', (event, path) => {
    event.preventDefault()
    newWorksheet(path)
  })
  // open all previously queued files
  for (let i=0; i < pathsToOpen.length; i++) {
    fs.access(pathsToOpen[i], (err) => {
     if (!err) { newWorksheet(pathsToOpen[i]) }
    })
  }
  // always open a window on startup
  if (pathsToOpen.length === 0) { newWorksheet() }
})

// The `app` object may receive an 'open-file' event *before* the 'ready' event.
// In this case, simply queue the filepath.
let pathsToOpen = []
// load an example worksheet right away on startup
if (TESTING) { pathsToOpen.push(appdir + '/../worksheets/Test.hhs') }
if (process.argv[2]) { pathsToOpen.push(process.argv[2]) }

let addPathToOpen = (event, path) => {
  event.preventDefault()
  pathsToOpen.push(path)
}
app.on('open-file', addPathToOpen)

// quit when all windows are closed
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') { app.quit() }
})

/* ****************************************************************
  "Preferences" window
**************************************************************** */
const prefPath    = app.getPath('userData') + '/preferences.json'
const prefDefault = {
  stackPath: ''
}

let loadPreferences = () => {
  try {
    json = JSON.parse(fs.readFileSync(prefPath, 'utf8'))
  } catch(err) {
    json = prefDefault
  }
  return json
}
let savePreferences = (prefs) => {
  fs.writeFileSync(prefPath, JSON.stringify(prefs), 'utf8')
}
let applyPreferences = (prefs) => {
  interpreter.main.setPaths(prefs.stackPath)
}

let prefWindow  = null
let menuPreferences = (item, focusedWindow) => {
  win = new BrowserWindow({x:20, y:20, width: 400, height: 150,
              webPreferences: {
                nodeIntegration: true,
                enableRemoteModule: true
              }})

  // remember global reference to window, due to garbage collection
  prefWindow = win
  win.on('closed', () => { prefWindow = null })

  win.loadURL('file://' + appdir + '/preferences.html')
  win.webContents.on('did-finish-load', () => {
    win.webContents.send('window-ready', loadPreferences())
  })
}


/* ****************************************************************
  Worksheet management
**************************************************************** */
let fileExtensions = [{ name: 'Worksheet' , extensions: ['hhs'] }]

// Global references to the window objects.
// Necessary, because a window will be closed if its JavaScript is garbage collected.
let windows = {}

// Create a new worksheet window and associate it to a path if necessary.
let newWorksheet = (path) => {
  let win = new BrowserWindow({x: 20, y: 20, width: 800, height: 600,
                  // FIXME: More security consciousness!
                  // While the interpreter can execute arbitrary Haskell actions,
                  // we may want to prevent dynamically loaded JavaScript
                  // from accessing the node.js environment.
                  webPreferences: {
                    nodeIntegration: true,
                    enableRemoteModule: true
                  }
                })
  let id  = win.id
  
  windows[id] = win    // keep a reference
  
  win.on('closed', () => {
    windows[id] = null // remove reference
    // kill associated interpreter
    // FIXME: Do we really need to send this signal, or does
    //        the Electron framework do that for us?
    interpreter.main.kill(id)
  })
  win.on('close', (event) => {
    // don't close the window if there have been unsaved changes.
    if (win.isDocumentEdited()) {
      const result = dialog.showMessageBox(win,
        {
          type   : "question",
          message: "Do you want to save the changes you made in the document?",
          details: "Your changes will be lost if you don't save them.",
          buttons:  ["Save", "Cancel", "Don't Save"],
        })
      switch (result) {
        case 0: win.saveFile(); break
        case 1: event.preventDefault(); break
        case 2: break
      }
    }
  })

  win.loadURL('file://' + appdir + '/worksheet.html')
  if (TESTING) { win.openDevTools({ detach : true }) }

  // make sure that this in an absolute path
  const filepath = path ? resolvePath(path) : ''
  win.setTitle('(untitled)')
  win.webContents.on('did-finish-load', () => {
    if (filepath) { win.setFilepath(filepath) }
    win.webContents.send('window-ready', filepath)
  })

  return win
}

let menuNew = (item, focusedWindow) => { newWorksheet() }

let menuOpen = (item, focusedWindow) => {
  dialog.showOpenDialog(null,
    {
      properties: ['openFile'],
      filters:    fileExtensions
    }, (paths) => {
      if (paths && paths.length > 0) { newWorksheet(paths[0]) }
  })
}

// NOTE: If we add new methods to an object with the help of `prototype`,
//       we have to use `function` in order to bind `this` to the correct value.
BrowserWindow.prototype.setFilepath = function (path) {
  app.addRecentDocument(path)
  this._filepath = path
  this.setRepresentedFilename(path)
  this.setTitle(require('path').basename(path))
}

BrowserWindow.prototype.getFilePath = function () {
  return this._filepath ? this._filepath : null;
}

BrowserWindow.prototype.saveFile = function () {
  let path = this.getFilePath()
  if (path) {
    this.webContents.send('save-file', path)
    this.setDocumentEdited(false)
  } else {
    this.saveFileAs()
  }
}

BrowserWindow.prototype.saveFileAs = function () {
  let win = this
  dialog.showSaveDialog(win,
      { filters: fileExtensions }, (path) => {
        if (path) {
            win.setFilepath(path)
            win.saveFile()
        }
  })
}

let menuSave   = (item, win) => { win.saveFile() }
let menuSaveAs = (item, win) => { win.saveFileAs() }


/* ****************************************************************
  Menu
**************************************************************** */

let handle = (name) => {
  return (item, focusedWindow) => { focusedWindow.webContents.send(name) }
}

let template = [
  { label: 'File',
    submenu: [
      { label: 'New', accelerator: 'CmdOrCtrl+N' , click: menuNew },
      { label: 'Open…', accelerator: 'CmdOrCtrl+O', click: menuOpen },
      { type: 'separator' },
      { label: 'Close', accelerator: 'CmdOrCtrl+W', role: 'close' },
      { label: 'Save', accelerator: 'CmdOrCtrl+S', click: menuSave },
      { label: 'Save As…', accelerator: 'Shift+CmdOrCtrl+S', click: menuSaveAs },
    ]
  },
  { label: 'Edit',
    submenu: [
      { role: 'undo' },
      { role: 'redo' },
      { type: 'separator' },
      { role: 'cut' },
      { role: 'copy' },
      { role: 'paste' },
      { role: 'selectall' },
    ]
  },
  { label: 'View',
    submenu: [
      { role: 'resetzoom' },
      { role: 'zoomin' },
      { role: 'zoomout' },
    ]
  },
  { label: 'Cells',
    submenu: [
      { label: 'Insert Evaluation Cell Above', click: handle('cell-insert-eval') },
      { label: 'Insert Text Cell Above', click: handle('cell-insert-text') },
      { type : 'separator' },
      { label: 'Delete Cell', click: handle('cell-delete') },
    ]
  },
  { label: 'Evaluation',
    submenu: [
      { label: 'Reload imports and options', accelerator: 'CmdOrCtrl+R', click: handle('reload-imports') },
      { type : 'separator' },
      { label: 'Evaluate Cell', accelerator: 'CmdOrCtrl+Enter', click: handle('evaluation-start') },
      { label: 'Interrupt Evaluation', accelerator: 'CmdOrCtrl+.', click: handle('evaluation-cancel') },
    ]
  },
]

if (process.platform == 'darwin') {
  let name = app.getName()
  template[0].submenu.splice(2,0,
    { label: 'Open Recent…', role: 'recentdocuments',
      submenu: [
        { type : 'separator' },
        { label: 'Clear Recent', role: 'clearrecentdocuments' }
      ]
    });
  template.unshift({
    label: name,
    submenu: [
      { role : 'about' },
      { type : 'separator' },
      { label: 'Preferences…', accelerator: 'Command+,', click: menuPreferences },
      { type : 'separator' },
      { role : 'services' },
      { type : 'separator' },
      { role : 'hide' },
      { role : 'hideothers' },  
      { role : 'unhide' },
      { type : 'separator' },
      { label: 'Quit', accelerator: 'Command+Q', click: () => { app.quit() } },
    ]
  })
} else {
  template[1].submenu.splice(template[1].submenu.length, 0,
    { type : 'separator' },
    { label: 'Preferences…', click: menuPreferences })
}
