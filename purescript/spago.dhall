{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "problem-counter"
, dependencies =
    [ "aff"
    , "avar"
    , "console"
    , "control"
    , "effect"
    , "node-readline"
    , "parsing"
    , "psci-support"
    , "read"
    ]
, packages =
    ./packages.dhall
}
