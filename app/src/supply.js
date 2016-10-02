/* *************************************************************
  A data structure for obtaining unique IDs
  (suitable for use with the HTML 'id' property)
************************************************************* */
var supply = 0;

exports.newId = function () {
  supply = supply + 1;
  return ("id-" + supply.toString());
};