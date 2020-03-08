library data_heytingalgebra_ffi;

dynamic boolConj(bool b1) {
  return (bool b2) {
    return b1 && b2;
  };
}

dynamic boolDisj(bool b1) {
  return (bool b2) {
    return b1 || b2;
  };
}

bool boolNot(bool b) {
  return !b;
}
