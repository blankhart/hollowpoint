library effect_dart_ffi;

dynamic pureE(a) {
  return () {
    return a;
  };
}

dynamic bindE(a) {
  return (f) {
    return () {
      return f(a())();
    };
  };
}

dynamic untilE(f) {
  return () {
    while (!f());
    return {};
  };
}

dynamic whileE(f) {
  return (a) {
    return () {
      while (f()) {
        a();
      }
      return {};
    };
  };
}

dynamic forE(lo) {
  return (hi) {
    return (f) {
      return () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
}

dynamic foreachE(as) {
  return (f) {
    return () {
      for (var i = 0, l = as.length; i < l; i++) {
        f(as[i])();
      }
    };
  };
}
