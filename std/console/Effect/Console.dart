import 'dart:html';

log(s) => () {
  window.console.log(s);
  return {};
};

warn(s) => () {
  window.console.warn(s);
  return {};
};

error(s) => () {
  window.console.error(s);
  return {};
};

info(s) => () {
  window.console.info(s);
  return {};
};

time(s) => () {
  window.console.time(s);
  return {};
};

timeLog(s) => () {
// FIXME: window.console.timeLog(s);
  throw UnimplementedError();
//  return {};
};

timeEnd(s) => () {
  window.console.timeEnd(s);
  return {};
};

clear() {
  window.console.clear({});
  return {};
}
