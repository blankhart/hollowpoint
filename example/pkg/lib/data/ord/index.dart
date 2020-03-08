// Generated by hollowpoint version 0.1.0
import './foreign.dart' as $foreign;
export './foreign.dart';
import 'package:pkg/data/eq/index.dart' as Data_Eq;
import 'package:pkg/data/ordering/index.dart' as Data_Ordering;
import 'package:pkg/data/ring/index.dart' as Data_Ring;
import 'package:pkg/data/semiring/index.dart' as Data_Semiring;
class Ord1 {
    final dynamic Eq10;
    final dynamic compare1;
    const Ord1(this.Eq10, this.compare1);
    static dynamic get create => (Eq10) {
        return (compare1) {
            return Ord1(Eq10, compare1);
        };
    };
}
class Ord {
    final dynamic Eq0;
    final dynamic compare;
    const Ord(this.Eq0, this.compare);
    static dynamic get create => (Eq0) {
        return (compare) {
            return Ord(Eq0, compare);
        };
    };
}
final ordVoid = Ord(() {
    return Data_Eq.eqVoid;
}, (v) {
    return (v1) {
        return Data_Ordering.EQ();
    };
});
final ordUnit = Ord(() {
    return Data_Eq.eqUnit;
}, (v) {
    return (v1) {
        return Data_Ordering.EQ();
    };
});
final ordString = Ord(() {
    return Data_Eq.eqString;
}, $foreign.ordStringImpl(Data_Ordering.LT())(Data_Ordering.EQ())(Data_Ordering.GT()));
final ordOrdering = Ord(() {
    return Data_Ordering.eqOrdering;
}, (v) {
    return (v1) {
        return (() {
            final $29 = v;
            final $30 = v1;
            if ($29 is Data_Ordering.LT) {
                if ($30 is Data_Ordering.LT) {
                    return Data_Ordering.EQ();
                };
            };
            if ($29 is Data_Ordering.EQ) {
                if ($30 is Data_Ordering.EQ) {
                    return Data_Ordering.EQ();
                };
            };
            if ($29 is Data_Ordering.GT) {
                if ($30 is Data_Ordering.GT) {
                    return Data_Ordering.EQ();
                };
            };
            if ($29 is Data_Ordering.LT) {
                return Data_Ordering.LT();
            };
            if ($29 is Data_Ordering.EQ) {
                if ($30 is Data_Ordering.LT) {
                    return Data_Ordering.GT();
                };
            };
            if ($29 is Data_Ordering.EQ) {
                if ($30 is Data_Ordering.GT) {
                    return Data_Ordering.LT();
                };
            };
            if ($29 is Data_Ordering.GT) {
                return Data_Ordering.GT();
            };
            throw FallThroughError();
        })();
    };
});
final ordNumber = Ord(() {
    return Data_Eq.eqNumber;
}, $foreign.ordNumberImpl(Data_Ordering.LT())(Data_Ordering.EQ())(Data_Ordering.GT()));
final ordInt = Ord(() {
    return Data_Eq.eqInt;
}, $foreign.ordIntImpl(Data_Ordering.LT())(Data_Ordering.EQ())(Data_Ordering.GT()));
final ordChar = Ord(() {
    return Data_Eq.eqChar;
}, $foreign.ordCharImpl(Data_Ordering.LT())(Data_Ordering.EQ())(Data_Ordering.GT()));
final ordBoolean = Ord(() {
    return Data_Eq.eqBoolean;
}, $foreign.ordBooleanImpl(Data_Ordering.LT())(Data_Ordering.EQ())(Data_Ordering.GT()));
compare1(dict) {
    return dict['compare1'];
}
compare(dict) {
    return dict['compare'];
}
comparing(dictOrd) {
    return (f) {
        return (x) {
            return (y) {
                return compare(dictOrd)(f(x))(f(y));
            };
        };
    };
}
greaterThan(dictOrd) {
    return (a1) {
        return (a2) {
            return (() {
                final v = compare(dictOrd)(a1)(a2);
                return (() {
                    final $31 = v;
                    if ($31 is Data_Ordering.GT) {
                        return true;
                    };
                    return false;
                })();
            })();
        };
    };
}
greaterThanOrEq(dictOrd) {
    return (a1) {
        return (a2) {
            return (() {
                final v = compare(dictOrd)(a1)(a2);
                return (() {
                    final $32 = v;
                    if ($32 is Data_Ordering.LT) {
                        return false;
                    };
                    return true;
                })();
            })();
        };
    };
}
signum(dictOrd) {
    return (dictRing) {
        return (x) {
            return (() {
                final $33 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing['Semiring0'](null)));
                if ($33) {
                    return Data_Semiring.one(dictRing['Semiring0'](null));
                };
                return Data_Ring.negate(dictRing)(Data_Semiring.one(dictRing['Semiring0'](null)));
            })();
        };
    };
}
lessThan(dictOrd) {
    return (a1) {
        return (a2) {
            return (() {
                final v = compare(dictOrd)(a1)(a2);
                return (() {
                    final $34 = v;
                    if ($34 is Data_Ordering.LT) {
                        return true;
                    };
                    return false;
                })();
            })();
        };
    };
}
lessThanOrEq(dictOrd) {
    return (a1) {
        return (a2) {
            return (() {
                final v = compare(dictOrd)(a1)(a2);
                return (() {
                    final $35 = v;
                    if ($35 is Data_Ordering.GT) {
                        return false;
                    };
                    return true;
                })();
            })();
        };
    };
}
max(dictOrd) {
    return (x) {
        return (y) {
            return (() {
                final v = compare(dictOrd)(x)(y);
                return (() {
                    final $36 = v;
                    if ($36 is Data_Ordering.LT) {
                        return y;
                    };
                    if ($36 is Data_Ordering.EQ) {
                        return x;
                    };
                    if ($36 is Data_Ordering.GT) {
                        return x;
                    };
                    throw FallThroughError();
                })();
            })();
        };
    };
}
min(dictOrd) {
    return (x) {
        return (y) {
            return (() {
                final v = compare(dictOrd)(x)(y);
                return (() {
                    final $37 = v;
                    if ($37 is Data_Ordering.LT) {
                        return x;
                    };
                    if ($37 is Data_Ordering.EQ) {
                        return x;
                    };
                    if ($37 is Data_Ordering.GT) {
                        return y;
                    };
                    throw FallThroughError();
                })();
            })();
        };
    };
}
ordArray(dictOrd) {
    return Ord(() {
        return Data_Eq.eqArray(dictOrd['Eq0'](null));
    }, (() {
        toDelta(x) {
            return (y) {
                return (() {
                    final v = compare(dictOrd)(x)(y);
                    return (() {
                        final $38 = v;
                        if ($38 is Data_Ordering.EQ) {
                            return 0;
                        };
                        if ($38 is Data_Ordering.LT) {
                            return 1;
                        };
                        if ($38 is Data_Ordering.GT) {
                            return Data_Ring.negate(Data_Ring.ringInt)(1);
                        };
                        throw FallThroughError();
                    })();
                })();
            };
        }
        return (xs) {
            return (ys) {
                return compare(ordInt)(0)($foreign.ordArrayImpl(toDelta)(xs)(ys));
            };
        };
    })());
}
final ord1Array = Ord1(() {
    return Data_Eq.eq1Array;
}, (dictOrd) {
    return compare(ordArray(dictOrd));
});
clamp(dictOrd) {
    return (low) {
        return (hi) {
            return (x) {
                return min(dictOrd)(hi)(max(dictOrd)(low)(x));
            };
        };
    };
}
between(dictOrd) {
    return (low) {
        return (hi) {
            return (x) {
                return (() {
                    final $39 = low;
                    final $40 = hi;
                    final $41 = x;
                    final low1 = $39;
                    final hi1 = $40;
                    final x1 = $41;
                    if (lessThan(dictOrd)(x1)(low1)) {
                        return false;
                    };
                    if (greaterThan(dictOrd)(x1)(hi1)) {
                        return false;
                    };
                    if (true) {
                        return true;
                    };
                    throw FallThroughError();
                })();
            };
        };
    };
}
abs(dictOrd) {
    return (dictRing) {
        return (x) {
            return (() {
                final $42 = greaterThanOrEq(dictOrd)(x)(Data_Semiring.zero(dictRing['Semiring0'](null)));
                if ($42) {
                    return x;
                };
                return Data_Ring.negate(dictRing)(x);
            })();
        };
    };
}
