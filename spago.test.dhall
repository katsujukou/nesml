{ name = "nesml"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "spec"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "uncurried-transformers"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
