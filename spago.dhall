{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "learn-ps"
, dependencies = [ "console", "effect", "psci-support" , "aff" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
