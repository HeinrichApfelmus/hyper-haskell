/* *************************************************************
    The "Preferences" window
************************************************************* */
const electron = require('electron')
const ipc      = electron.ipcRenderer
const jQuery   = require('./lib/jquery-3.1.0.js')
const $ = jQuery

ipc.on('window-ready', (event, prefs) => {
  // show current preferences
  $('#stackPath').val(prefs.stackPath)
  
  let save = () => {
    ipc.send('save-preferences', {
      stackPath: $('#stackPath').val()
    })
  }
  
  // save preferences on enter
  $('form').submit( (event) => {
    save()
    event.preventDefault()
  })
  // save on close as well
  window.onbeforeunload = save
})
