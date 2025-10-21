/* *************************************************************
    The worksheet window
************************************************************* */
const interpreter = window.newInterpreter()
const fileformat = window.fileformat

/* *************************************************************
  Window setup
************************************************************* */

// Initial entry point, sent by the main process
window.electron.onWindowReady( (path) => {
  // initialize interpreter code
  interpreter.init(window)

  let cells = NewCells($('#cells'))
  cells.setCells(fileformat.single().cells)

  const reloadImports = async () => {
    const cwd = await window.electron.getCurrentWorkingDirectory()

    // tell interpreter to load imports
    $('#status').empty()
    interpreter.loadImports({
      cwd         : cwd,
      searchPath  : $('#searchPath').val(),
      packageTool : $('#packageTool').val(),
      packagePath : $('#packagePath').val(),
	    extensions  : cmExtensions.getDoc().getValue().split(','),
      imports     : cmImportModules.getDoc().getValue().split('\n'),
      files       : cmLoadFiles.getDoc().getValue().split('\n'),
    }, (result) => {
      if (result.status === 'ok') {
        $('#status').text('Imports loaded ok.')
      } else {
        $('#status').text('Could not load imports: ')
        $('#status').append($('<pre/>').text(result.errors.toString()))
      }
    })
  }

  const fromMaybe = (def,x) => { return x ? x : def }

  const loadFile = async (path) => {
    // FIXME: Better error reporting when loading from a file fails
    const data = await window.electron.fs.readFileSync(path, 'utf8')
    const json = fileformat.update('0.2.1.0', fileformat.parse(data))

    cmExtensions.getDoc().setValue(fromMaybe('', json.extensions))
    cmImportModules.getDoc().setValue(fromMaybe('', json.importModules))
    cmLoadFiles.getDoc().setValue(fromMaybe('', json.loadFiles))
    $("#searchPath").val(json.settings.searchPath)
    $("#packagePath").val(json.settings.packagePath)
    $("#packageTool").val(json.settings.packageTool)
    cells.setCells(json.cells)
    window.electron.setDocumentEdited(false)
  }
  const init = async () => {
    if (path) { await loadFile(path) }
    await reloadImports()
  }
  init() // fire off the promise

  /* NOTE [SemanticVersioning]

  When it comes to semantic versioning, the conventions between Haskell and npm differ.
    Haskell: major.major.minor.patch  e.g. 0.1.0.0
    npm    : major.minor.patch        e.g.   1.0.0
  The Haskell scheme was chosen to allow a major versions starting with 0, e.g. `0.7`
  and the first "solid" release being e.g. `1.0`.
  It appears that many projects built with packages on npm, e.g. Electron itself,
  do not follow semantics versioning precisely whenever the major version is 0.
  There is even a special case in the specification for that:

  " 4. Major version zero (0.y.z) is for initial development.
    Anything may change at any time. The public API should not be considered stable. "

  Oh well...

  Whenever possible, we use the Haskell semantic versioning scheme.
  If we have to align with the npm scheme, we translate Haskell semvers into npm semvers
  by using hundreds for the first major version number, that is

    A.B.C.D --> 100*A+B.C.D
    0.3.0.1 -->       3.0.1
    1.2.0.4 -->     102.0.4
  */
  window.electron.onSaveFile( (path) => {
    const json = {
      version        : '0.2.1.0',
      cells          : cells.getCells(),
      extensions     : cmExtensions.getDoc().getValue(),
      importModules  : cmImportModules.getDoc().getValue(),
      loadFiles      : cmLoadFiles.getDoc().getValue(),
      settings       : {
        packageTool : $('#packageTool').val(),
        packagePath : $('#packagePath').val(),
        searchPath  : $('#searchPath').val(),
      },
    }
    window.electron.fs.writeFileSync(path, fileformat.stringify(json), 'utf8')
  })

  // FIXME: Call  setDocumentEdited()  also when settings are changed
  window.electron.onCellInsertEval( () => {
    cells.insertBeforeCurrent('code')
    window.electron.setDocumentEdited(true)
  })
  window.electron.onCellInsertText( () => {
    cells.insertBeforeCurrent('text')
    window.electron.setDocumentEdited(true)
  })
  window.electron.onCellDelete(() => {
    cells.removeCurrent()
    window.electron.setDocumentEdited(true)
  })

  window.electron.onReloadImports(reloadImports)
  window.electron.onEvaluationStart(cells.evaluateCurrent)
  window.electron.onEvaluationCancel(interpreter.cancel)
})

/* *************************************************************
    Display interpreter results
************************************************************* */
const formatResult = (element, result) => {
  if (result.status === 'ok') {
    if (result.value.type === 'string') {
      $(element).text(result.value.value)
    } else if (result.value.type === 'html') {
      $(element).html(result.value.value)
    }
  } else {
    $(element).empty().append($("<pre>").text(result.errors.join('\n')))
  }
}

/* *************************************************************
    Global supply of IDs
************************************************************* */
let totalSupply = 0;

const supply = {}
supply.newId = () => {
  totalSupply = totalSupply + 1
  return ("id-" + totalSupply.toString())
}

/* *************************************************************
    Manage a list of cells
************************************************************* */

const NewCells = (parent) => {
  let that  = {}
  let cells = window.newSequence()

  let focus           = -1
  const currentIsLast = () => { return (focus === cells.length() - 1) }
  that.current        = () => { return cells.at(focus) }

  // set focus on a particular cell
  const setFocus = (index) => {
    if (0 <= focus && focus < cells.length()) {
      cells.at(focus).focus(false)
    }
    focus = index
    cells.at(focus).focus(true)
  }
  // update focus if necessary
  const updateFocus = (cell) => {
    const index = cells.index(cell)
    if (focus !== index) { setFocus(index) }
  }

  // move the cursor up or down
  // return value: the cursor did move
  const move = (cell, delta, ch) => {
    const index  = cells.index(cell)
    const index2 = index + delta
    // move cursor to a new cell if it exists
    if (cells.at(index2)) {
      setFocus(index2)
      if (delta >= 1) { // from below
        cells.at(index2).setCursor({ line: 0, ch: ch })
      } else if (delta <= -1) { // from above
        cells.at(index2).setCursor({ line: cells.at(index2).lineCount()-1, ch: ch})
      }
      return true
    } else { return false }
  }

  // Initialize from an array of expressions.
  that.setCells = (xs) => {
    cells.empty()
    focus = -1
    parent.empty()
    for (let i=0; i<xs.length; i++) {
      that.appendCell(xs[i]['cell_type']).setValue(xs[i]['source'])
    }
    // add an empty cell at the end if necessary
    if (xs.length > 0 && xs[xs.length-1]['source'] !== '') {
      that.appendCell('code').setValue('')
    }
    setFocus(0)
  }

  // Retrieve the represented expressions.
  that.getCells = () => {
    let result = ['']
    for (let i=0; i<cells.length(); i++) {
      result[i] = {
        'cell_type' : cells.at(i).cell_type,
        'source'    : cells.at(i).getValue(),
      }
    }
    // remove empty cells from the end
    let i = result.length-1
    while (i>0 && result[i]['source'] === '') { i--; }
    result = result.slice(0,i+1)
    return result
  }

  // Create cells
  // Create a new evaluation cell at the end.
  that.appendCell = (cell_type) => {
    const insertDOM = (el) => { parent.append(el) }
    const cell = ( cell_type === 'code' ?
      NewEvaluationCell(insertDOM, move) : NewTextCell(insertDOM, move) )
    cell.on('focus', () => { updateFocus(cell) })
    cells.push(cell)
    return cell
  }
  // Create cell and insert before current cell
  that.insertBeforeCurrent = (cell_type) => {
    if (focus >= 0) {
      const insertDOM = (el) => { cells.at(focus).dom().before(el) }
      const cell = ( cell_type === 'code' ?
        NewEvaluationCell(insertDOM, move) : NewTextCell(insertDOM, move) )
      cell.on('focus', () => { updateFocus(cell) })

      cells.at(focus).focus(false)
      cells.insertBefore(cell, focus)
      setFocus(focus)
      return cell
    }
  }

  // Delete the current cell
  that.removeCurrent = () => {
    if (focus >= 0) {
      let index = focus
      // remove cell from DOM
      cells.at(index).focus(false)
      cells.at(index).remove()
      cells.remove(index)

      // set new focus
      if (index < cells.length() ) {
        setFocus(index)
      } else {
        setFocus(cells.length() - 1)
      }
    }
  }

  // Evaluate the current cell and move the focus to the next one.
  that.evaluateCurrent = () => {
    if (focus >= 0) {
      cells.at(focus).evaluate()
      if (currentIsLast()) { that.appendCell('code') }
      setFocus(focus + 1)
    }
  }

  return that
}

/* *************************************************************
    Text cell
************************************************************* */
const NewTextCell = (insertDOM, move) => {
  let that = {}
  that.cell_type = 'text'

  // create DOM elements
  const div   = $("<div class='cell text'></div>")
  insertDOM(div)
  const div2  = $("<div></div>")
  div2.appendTo(div)
  const quill = new Quill(div2.get(0))

  that.dom    = () => { return div }
  that.remove = () => { div.detach() }

  that.setValue  = (s) => { quill.setText(s) }
  that.getValue  = ()  => { return quill.getText() }
  that.lineCount = ()  => { return 1 }

  that.evaluate  = ()  => { } // do nothing

  // signal that the document has been edited
  quill.on('text-change', () => { window.electron.setDocumentEdited(true) })

  // Focus and cursor management
  that.on = (event, fun) => {
    if (event === 'focus') {
      quill.on('selection-change', (range) => { if (range) { fun() } })
    }}
  that.setCursor = (cursor) => {
    if (cursor.line === 0) { quill.setSelection(0,0) }
    if (cursor.line >   0) { quill.setSelection(quill.getLength()-1,0) }
  }
  that.focus = (bool) => {
    // focus or unfocus the cell
    div.toggleClass('focus', bool)
    if (bool) { quill.getSelection(true) } // looks funny, but this focuses the editor
  }

  div.on('keydown', (event) => {
    // moving the cursor "out" of the cell will seamlessly move to the next cell
    if (event.keyCode === 38 && !event.shiftKey) { // if (up key) {
      // hack to find out whether the cursor is at the top of the editor
      const atTop = (quill.getBounds(0).top === quill.getBounds(quill.getSelection()).top)
      if (atTop) {
        quill.setSelection(0)   // remove any selection
        move(that, -1, 0)
        event.preventDefault()
      }
    } else if (event.keyCode === 40 && !event.shiftKey) { // if (down key) {
      const atBottom = (quill.getBounds(quill.getLength()).top === quill.getBounds(quill.getSelection()).top)
      if (atBottom) {
        if (move(that, 1, 0)) {
          quill.setSelection(null)  // remove any selection
          event.preventDefault()
        }
      }
    }})

  return that
}

/* *************************************************************
    Evaluation cell
************************************************************* */
const NewEvaluationCell = (insertDOM, move) => {
  let that = {}
  that.cell_type = 'code'

  // create DOM elements and CodeMirror editor
  const div = $("<div class='cell eval'></div>")
  insertDOM(div)
  const cm = CodeMirror( (el) => { $(el).appendTo(div) } )
  cm.setOption('indentUnit', 4)
  cm.setOption('extraKeys', { Tab: betterTab })
  const out = $("<div class='out' id='" + supply.newId() + "'></div>")
  out.appendTo(div)

  that.dom    = () => { return div }    // return associated DOM element
  that.remove = () => { div.detach() }

  that.setValue  = (s) => { cm.getDoc().setValue(s) }
  that.getValue  = ()  => { return cm.getDoc().getValue() }
  that.lineCount = ()  => { return cm.getDoc().lineCount() }

  that.evaluate  = ()  => {
    // evaluate the cell
    div.addClass('evaluating')
    out.empty()
    out.show()
    interpreter.eval(cm.getDoc().getValue(), (result) => {
      div.removeClass('evaluating')
      formatResult(out, result)
    })
  }

  // signal that the document has been edited
  cm.on('changes', () => { window.electron.setDocumentEdited(true) })

  // Focus and cursor management
  that.on = (event, fun) => {
    if (event === 'focus') { cm.on('focus', fun) }
  }
  that.setCursor = (x)    => { cm.getDoc().setCursor(x) }
  that.focus     = (bool) => {
    div.toggleClass('focus', bool)
    if (bool) { cm.focus() }
  }

  cm.on('keydown', (instance, event) => {
    // moving the cursor "out" of the cell will seamlessly move to the next cell
    let doc = cm.getDoc()
    let ch  = doc.getCursor().ch
    if (event.keyCode === 38 && !event.shiftKey) { // if(up key) {
      if (doc.getCursor().line <= 0) {
        doc.setCursor(0,0)    // remove any selection
        move(that, -1, ch)
        event.preventDefault()
      }
    } else if (event.keyCode === 40 && !event.shiftKey) { // if(down key) {
      if (doc.getCursor().line + 1 >= doc.lineCount()) {
        if (move(that, 1, ch)) {
          doc.setCursor(0,0)  // remove any selection
          event.preventDefault()
        }
      }
    }})

  return that
}


/* *************************************************************
    CodeMirror
************************************************************* */
const betterTab = (cm) => {
  if (cm.somethingSelected()) {
    cm.indentSelection('add')
  } else {
    cm.execCommand('insertSoftTab')
  }
}
