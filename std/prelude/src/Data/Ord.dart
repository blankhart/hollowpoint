library data_ord_ffi;

dynamic unsafeCompareImpl(lt) {
  return (eq) {
    return (gt) {
      return (x) {
        return (y) {
          return x < y ? lt : x == y ? eq : gt;
        };
      };
    };
  };
}

final dynamic ordBooleanImpl = unsafeCompareImpl;
final dynamic ordIntImpl = unsafeCompareImpl;
final dynamic ordNumberImpl = unsafeCompareImpl;
final dynamic ordStringImpl = unsafeCompareImpl;
final dynamic ordCharImpl = unsafeCompareImpl;

final dynamic ordArrayImpl = (f) {
  return (xs) {
    return (ys) {
      var i = 0;
      var xlen = xs.length;
      var ylen = ys.length;
      while (i < xlen && i < ylen) {
        var x = xs[i];
        var y = ys[i];
        var o = f(x)(y);
        if (o != 0) {
          return o;
        }
        i++;
      }
      if (xlen == ylen) {
        return 0;
      } else if (xlen > ylen) {
        return -1;
      } else {
        return 1;
      }
    };
  };
};
