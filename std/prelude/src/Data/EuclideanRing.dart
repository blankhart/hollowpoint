import 'dart:math';

intDegree(num x) => min(x.abs(), 2147483647);

// See the Euclidean definition in
// https://en.m.wikipedia.org/wiki/Modulo_operation.
intDiv(x) => (y) {
  if (y == 0) return 0;
  return y > 0 ? (x / y).floor() : -(x / -y).floor();
};

intMod(x) => (y) {
  if (y == 0) return 0;
  var yy = y.abs();
  return ((x % yy) + yy) % yy;
};

numDiv(n1) => (n2) => n1 / n2;
