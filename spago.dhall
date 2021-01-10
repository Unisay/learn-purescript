{ name = "learn-ps"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "filterable"
  , "foldable-traversable"
  , "generics-rep"
  , "lazy"
  , "naturals"
  , "node-readline"
  , "partial"
  , "psci-support"
  , "pseudo-random"
  , "refs"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
