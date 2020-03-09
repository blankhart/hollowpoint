{ name =
    "purescript-ballista"
, backend =
    "cabal run psd"
, dependencies =
    [ "effect", "console", "psci-support" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs" ]
}
