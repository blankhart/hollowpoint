library data_eq_ffi;

dynamic refEq(r1) {
  return (r2) {
    return r1 == r2;
  };
}

final eqBooleanImpl = refEq;
final eqIntImpl = refEq;
final eqNumberImpl = refEq;
final eqCharImpl = refEq;
final eqStringImpl = refEq;

dynamic eqArrayImpl(f) {
  return (xs) {
    return (ys) {
      if (xs == ys) return true;
      if (xs.length != ys.length) return false;
      for (var i = 0; i < xs.length; i++) {
        if (!f(xs[i])(ys[i])) return false;
      }
      return true;
    };
  };
}
