library data_show_ffi;

String showIntImpl(n) => n.toString();

// FIXME: Likely add trailing zero.
String showNumberImpl(n) {
  var str = n.toString();
  // return (str + ".0").isNaN() ? str : str + ".0";
  return str;
}

//  FIXME: Investigate further the intended behavior and the relationship to
//  Dart runes/strings.
String showCharImpl(c) {
  var code = c.codeUnitAt(0);
  if (code < 0x20 || code == 0x7F) {
    switch (c) {
      case "\x07": return "'\\a'";
      case "\b": return "'\\b'";
      case "\f": return "'\\f'";
      case "\n": return "'\\n'";
      case "\r": return "'\\r'";
      case "\t": return "'\\t'";
      case "\v": return "'\\v'";
    }
    return "'\\" + code.toString() + "'";
  }
  return c == "'" || c == "\\" ? "'\\" + c + "'" : "'" + c + "'";
}

//  FIXME: Investigate and figure out whether it is necessary to use character
//  substitutions similar to the PureScript implementation.
//  RegExp _exp = new RegExp(...);
String showStringImpl(String s) {
  return "\"$s\"";
}

dynamic showArrayImpl<T>(String Function(T) f) {
  return (List<T> xs) {
    final ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
}

dynamic cons<T>(T head) {
  return (List<T> tail) {
    return [head, ...tail];
  };
}

dynamic join<T>(String separator) {
  return (List<T> xs) {
    return xs.join(separator);
  };
}
