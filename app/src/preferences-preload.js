/* ****************************************************************************
    Setting up IPC for the browser window
**************************************************************************** */

const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electron', {

  /* Events */

  onWindowReady: (callback) => ipcRenderer.on(
      'window-ready', (event, ...args) => callback(...args)
  ),

  /* Functions */

  savePreferences: (...args) => {
    ipcRenderer.send('save-preferences', ...args)
  }
})
