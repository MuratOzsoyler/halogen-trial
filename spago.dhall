{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "event"
    , "exceptions"
    , "halogen"
    , "halogen-css"
    , "psci-support"
    , "random"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}