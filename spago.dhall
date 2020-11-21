{ name = "learn-ps"
, dependencies =
  [ "console"
  , "control"
  , "debug"
  , "effect"
  , "filterable"
  , "psci-support"
  , "pseudo-random"
  , "aff"
  , "naturals"
  , "node-readline"
  , "generics-rep"
  , "partial"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
