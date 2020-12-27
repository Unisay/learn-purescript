{ name = "learn-ps"
, dependencies =
  [ "console"
  , "control"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "filterable"
  , "psci-support"
  , "pseudo-random"
  , "aff"
  , "lazy"
  , "naturals"
  , "node-readline"
  , "generics-rep"
  , "partial"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
