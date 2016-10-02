/* *************************************************************
    Visual enhancements for the worksheet window
************************************************************* */

$(document).ready(() => {
  $('h1').click(function () {
    $(this).toggleClass('closed')
    // hide/show the next elements, which should be <div> containing the section
    $(this).next().toggle()
  })
})