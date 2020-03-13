library control_apply_ffi;

dynamic arrayApply<T>(List<dynamic Function(T)> fs) {
  return (List<T> xs) {
    final l = fs.length;
    final k = xs.length;
    final result = List(l*k);
    var n = 0;
    for (var i = 0; i < l; i++) {
      final f = fs[i];
      for (var j = 0; j < k; j++) {
        result[n++] = f(xs[j]);
      }
    }
    return result;
  };
}
