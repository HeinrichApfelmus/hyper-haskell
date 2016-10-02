/* *************************************************************
  A data structure for storing sequences of things
  
  Currently a very simple-minded implementation as an array.
************************************************************* */
exports.newSequence = function () {
  var xs   = [];
  var that = {};
  
  that.at = function (index) { return xs[index]; };
  that.length = function () { return xs.length; };
  that.index  = function (obj) {
    for (var i=0; i < xs.length; i++ ) {
      if (xs[i] === obj) { return i; }
    }
    return null;
  };
  
  that.push  = function (obj) { xs.push(obj); };
  that.empty = function () { xs = []; };
  that.insertBefore = function (obj, index) {
    xs.splice(index, 0, obj);
  };
  that.remove = function (index) {
    if (0 <= index && index < xs.length) {
      xs.splice(index, 1);
    }
  };
  
  return that;
};
