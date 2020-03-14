# Notes

```dart

final main = (() {
    final b = 6;
    final a = 5;
    return
      // returns a function that applies discardUnit.discard
      // that is a function taking a Bind typeclass and invokes bind.
      Control_Bind.discard(Control_Bind.discardUnit)
        // bindEffect is a typeclass instance for Effect.
        // bindE(a) => (f) => () => f(a())();
        (Effect.bindEffect)
        // bindEffect argument a
        (Data_Function.apply(Effect_Print.print)(Data_Semigroup.append(Data_Semigroup.semigroupString)("Eleven: ")(Data_Show.show(Data_Show.showInt)(Data_Semiring.add(Data_Semiring.semiringInt)(5)(6))))
        )
        // bindEffect argument f
        // a() is invoked as $dollar__unused, calling the result
        (($dollar__unused) {
            print("Holy smokes!");
            return Control_Bind.discard(Control_Bind.discardUnit)(Effect.bindEffect)
            // bindEffect argument a
            (Effect_Print.print("Hello."))
            // bindEffect argument f
            (($dollar__unused) {
                return Data_Function.apply(Effect_Print.print)(Data_Semigroup.append(Data_Semigroup.semigroupString)("Underscored: ")(Data_Show.show(Data_Show.showInt)(Other.underscored)));
            });
        });
})();

```
