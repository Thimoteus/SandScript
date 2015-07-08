// module SandScript.Util

exports.str2num = function str2num(str) {
  return (str | 0);
}

exports.unwords = function unwords(arr) {
  return arr.join(" ");
}
