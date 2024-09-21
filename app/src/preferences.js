/* *************************************************************
    The "Preferences" window
************************************************************* */

window.electron.onWindowReady( (prefs) => {
  // show current preferences
  $('#stackPath').val(prefs.stackPath)
  
  const save = () => {
    window.electron.savePreferences({
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
