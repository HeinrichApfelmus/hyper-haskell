/* ****************************************************************************
    Setting up IPC for the browser window
**************************************************************************** */

const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electron', {

  /* Events */

  onCellDelete: (callback) => ipcRenderer.on(
    'cell-delete', (event, ...args) => callback(...args)
  ),
  onCellInsertEval: (callback) => ipcRenderer.on(
    'cell-insert-eval', (event, ...args) => callback(...args)
  ),
  onCellInsertText: (callback) => ipcRenderer.on(
    'cell-insert-text', (event, ...args) => callback(...args)
  ),
  onDoneInterpreterStart: (callback) => ipcRenderer.on(
    'done-interpreter-start', (event, ...args) => callback(...args)
  ),
  onEvaluationStart: (callback) => ipcRenderer.on(
    'evaluation-start', (event, ...args) => callback(...args)
  ),
  onEvaluationCancel: (callback) => ipcRenderer.on(
    'evaluation-cancel', (event, ...args) => callback(...args)
  ),
  onReloadImports: (callback) => ipcRenderer.on(
    'reload-imports', (event, ...args) => callback(...args)
  ),
  onSaveFile: (callback) => ipcRenderer.on(
    'save-file', (event, ...args) => callback(...args)
  ),
  onWindowReady: (callback) => ipcRenderer.on(
    'window-ready', (event, ...args) => callback(...args)
  ),

  /* Functions */

  setDocumentEdited: (...args) => {
    // Note [setDocumentEdited]
    // We do not use the 'remote' object here,
    // because doing so while handling a CodeMirror 'changes' event
    // will lead to a duplicate Enter keypress. I have no idea why,
    // but using IPC to call 'setDocumentEdited' solves the issue.
    ipcRenderer.send('setDocumentEdited', ...args)
  },
  startInterpreter: (...args) => {
    ipcRenderer.send('startInterpreter', ...args)
  },
  getRepresentedFilename: (...args) => {
    return ipcRenderer.invoke('getRepresentedFilename', ...args)
  },
  getCurrentWorkingDirectory: (...args) => {
    return ipcRenderer.invoke('getCurrentWorkingDirectory', ...args)
  },
  fs: {
    readFileSync: (...args) => {
      return ipcRenderer.invoke('readFileSync', ...args)
    },
    writeFileSync: (...args) => { ipcRenderer.send('writeFileSync', ...args) }
  }

})
