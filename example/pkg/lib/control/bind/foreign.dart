library control_bind_ffi;

dynamic arrayBind<T>(List<T> arr) {
  return (List<T> Function(T) f) {
    final result = [];
    for (var i = 0, l = arr.length; i < l; i++) {
      result.addAll(f(arr[i]));
    }
    return result;
  };
}
