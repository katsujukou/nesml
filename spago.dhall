{ name = "nesml"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "node-fs-aff"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "profunctor-lenses"
  , "record"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "uncurried-transformers"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
