{ name =
    "purescript-ballista"
, backend =
    "cabal run psd -- --foreigns=../std/*/src/ --foreigns=src/ --main-is=Main --run"
, dependencies =
    [ "effect", "console", "dart-print", "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
