{ name = "learn-ps"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "custom-prelude"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "free"
  , "identity"
  , "integers"
  , "lcg"
  , "machines"
  , "maybe"
  , "motsunabe"
  , "naturals"
  , "newtype"
  , "node-readline"
  , "nonempty"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "pseudo-random"
  , "random"
  , "record"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unicode"
  , "unordered-collections"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purs-backend-es build"
}
