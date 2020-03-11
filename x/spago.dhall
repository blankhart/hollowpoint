{ name =
    "purescript-ballista"
, backend =
    "cabal run psd -- --foreigns=../std/*/"
, dependencies =
    [ "effect", "console", "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
