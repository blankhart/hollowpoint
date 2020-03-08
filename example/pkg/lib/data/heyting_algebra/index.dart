// Generated by hollowpoint version 0.1.0
import './foreign.dart' as $foreign;
export './foreign.dart';
import 'package:pkg/data/unit/index.dart' as Data_Unit;
class HeytingAlgebra {
    final dynamic conj;
    final dynamic disj;
    final dynamic ff;
    final dynamic implies;
    final dynamic not;
    final dynamic tt;
    const HeytingAlgebra(this.conj, this.disj, this.ff, this.implies, this.not, this.tt);
    static dynamic get create => (conj) {
        return (disj) {
            return (ff) {
                return (implies) {
                    return (not) {
                        return (tt) {
                            return HeytingAlgebra(conj, disj, ff, implies, not, tt);
                        };
                    };
                };
            };
        };
    };
}
tt(dict) {
    return dict['tt'];
}
not(dict) {
    return dict['not'];
}
implies(dict) {
    return dict['implies'];
}
final heytingAlgebraUnit = HeytingAlgebra((v) {
    return (v1) {
        return Data_Unit.unit;
    };
}, (v) {
    return (v1) {
        return Data_Unit.unit;
    };
}, Data_Unit.unit, (v) {
    return (v1) {
        return Data_Unit.unit;
    };
}, (v) {
    return Data_Unit.unit;
}, Data_Unit.unit);
ff(dict) {
    return dict['ff'];
}
disj(dict) {
    return dict['disj'];
}
final heytingAlgebraBoolean = HeytingAlgebra($foreign.boolConj, $foreign.boolDisj, false, (a) {
    return (b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
}, $foreign.boolNot, true);
conj(dict) {
    return dict['conj'];
}
heytingAlgebraFunction(dictHeytingAlgebra) {
    return HeytingAlgebra((f) {
        return (g) {
            return (a) {
                return conj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, (f) {
        return (g) {
            return (a) {
                return disj(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, (v) {
        return ff(dictHeytingAlgebra);
    }, (f) {
        return (g) {
            return (a) {
                return implies(dictHeytingAlgebra)(f(a))(g(a));
            };
        };
    }, (f) {
        return (a) {
            return not(dictHeytingAlgebra)(f(a));
        };
    }, (v) {
        return tt(dictHeytingAlgebra);
    });
}
