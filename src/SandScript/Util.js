// module SandScript.Util

exports.str2num = function str2num(str) {
  return (str | 0);
}

exports.unwordsArr = function unwordsArr(arr) {
  return arr.join(" ");
}
