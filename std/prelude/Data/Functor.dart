library data_functor_ffi;

dynamic arrayMap(f) {
  return (arr) {
    final l = arr.length;
    final result = List(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
}
