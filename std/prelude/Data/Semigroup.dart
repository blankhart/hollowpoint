library data_semigroup_ffi;

dynamic concatString(s1) {
  return (s2) {
    return s1 + s2;
  };
}

dynamic concatArray(xs) {
  return (ys) {
    if (xs.length == 0) return ys;
    if (ys.length == 0) return xs;
    return xs.concat(ys);
  };
}
