{ name =
    "purescript-example"
, backend =
    -- --run omitted
    "cabal run psd -- --foreigns=../std/*/src/ --foreigns=src/ --main-is=Main"
, dependencies =
    [ "effect", "dart-flutter", "dart-print", "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
