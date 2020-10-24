{ name = "learn-ps"
, dependencies = [ "console", "effect", "psci-support", "aff", "node-readline" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
