library effect_console_ffi;

// import 'dart:html';

dynamic log(String s) {
  return () {
    print(s);
    return {};
  };
}

/*
dynamic warn = function (s) {
  return function () {
    console.warn(s);
    return {};
  }
}

dynamic error = function (s) {
  return function () {
    console.error(s);
    return {};
  };
};

dynamic info = function (s) {
  return function () {
    console.info(s);
    return {};
  };
};

dynamic time = function (s) {
  return function () {
    console.time(s);
    return {};
  };
};

dynamic timeLog = function (s) {
  return function () {
    console.timeLog(s);
    return {};
  };
};

dynamic timeEnd = function (s) {
  return function () {
    console.timeEnd(s);
    return {};
  };
};

dynamic clear = function () {
  console.clear();
  return {};
};

*/
