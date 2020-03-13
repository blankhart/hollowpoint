
unsafeCompareImpl(lt) => (eq) => (gt) => (x) => (y) {
  return x < y ? lt : x == y ? eq : gt;
} as dynamic;
