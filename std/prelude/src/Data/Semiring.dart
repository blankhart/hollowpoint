library data_semiring_ffi;

dynamic intAdd(x) {
  return (y) {
    return x + y;
  };
}

dynamic intMul(x) {
  return (y) {
    return x * y;
  };
}

dynamic numAdd(n1) {
  return (n2) {
    return n1 + n2;
  };
}

dynamic numMul(n1) {
  return (n2) {
    return n1 * n2;
  };
}
