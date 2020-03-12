mkEffectFn1(fn) => (x) =>
  fn(x)();

mkEffectFn2(fn) => (a, b) =>
  fn(a)(b)();

mkEffectFn3(fn) => (a, b, c) =>
  fn(a)(b)(c)();

mkEffectFn4(fn) => (a, b, c, d) =>
  fn(a)(b)(c)(d)();

mkEffectFn5(fn) => (a, b, c, d, e) =>
  fn(a)(b)(c)(d)(e)();

mkEffectFn6(fn) => (a, b, c, d, e, f) =>
  fn(a)(b)(c)(d)(e)(f)();

mkEffectFn7(fn) => (a, b, c, d, e, f, g) =>
  fn(a)(b)(c)(d)(e)(f)(g)();

mkEffectFn8(fn) => (a, b, c, d, e, f, g, h) =>
  fn(a)(b)(c)(d)(e)(f)(g)(h)();

mkEffectFn9(fn) => (a, b, c, d, e, f, g, h, i) =>
  fn(a)(b)(c)(d)(e)(f)(g)(h)(i)();

mkEffectFn10(fn) => (a, b, c, d, e, f, g, h, i, j) =>
  fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)();

runEffectFn1(fn) => (a) => () =>
  fn(a);

runEffectFn2(fn) => (a) => (b) => () =>
  fn(a, b);

runEffectFn3(fn) => (a) => (b) => (c) => () =>
  fn(a, b, c);

runEffectFn4(fn) => (a) => (b) => (c) => (d) => () =>
  fn(a, b, c, d);

runEffectFn5(fn) => (a) => (b) => (c) => (d) => (e) => () =>
  fn(a, b, c, d, e);

runEffectFn6(fn) => (a) => (b) => (c) => (d) => (e) => (f) => () =>
  fn(a, b, c, d, e, f);

runEffectFn7(fn) => (a) => (b) => (c) => (d) => (e) => (f) => (g) => () =>
  fn(a, b, c, d, e, f, g);

runEffectFn8(fn) => (a) => (b) => (c) => (d) => (e) => (f) => (g) => (h) => () =>
  fn(a, b, c, d, e, f, g, h);

runEffectFn9(fn) => (a) => (b) => (c) => (d) => (e) => (f) => (g) => (h) => (i) => () =>
  fn(a, b, c, d, e, f, g, h, i);

runEffectFn10(fn) => (a) => (b) => (c) => (d) => (e) => (f) => (g) => (h) => (i) => (j) => () =>
  fn(a, b, c, d, e, f, g, h, i, j);
