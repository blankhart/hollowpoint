library effect_dart_ffi;

pureE(a) => () => a;

bindE(a) => (f) => () => f(a())();

untilE(f) => () {
  while (!f());
  return {};
};

whileE(f) => (a) => () {
  while (f()) a();
  return {};
};

forE(lo) => (hi) => (f) => () {
  for (var i = lo; i < hi; i++) {
    f(i)();
  }
};

foreachE(as) => (f) => () {
  for (var i = 0, l = as.length; i < l; i++) {
    f(as[i])();
  }
};
