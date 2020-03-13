{ name =
    "purescript-ballista"
, backend =
    "cabal run psd -- --foreigns=../std/*/ --foreigns=src/ --main-is=Main --run"
, dependencies =
    [ "effect", "console", "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
