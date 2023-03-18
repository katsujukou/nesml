module Language.Nesml.Parsing.Errors
  ( ParserError
  , ParserErrorType(..)
  , rangedError
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Language.Nesml.Parsing.Types (SourceRange, Token)

data ParserErrorType
  = MissingCloseComment
  | MissingCloseDoubleQuote
  | UnexpectedCloseComment

derive instance Eq ParserErrorType
derive instance Ord ParserErrorType
derive instance Generic ParserErrorType _
instance Show ParserErrorType where
  show = genericShow

type ParserError =
  { range :: SourceRange
  , toks :: Array Token
  , type :: ParserErrorType
  }

rangedError :: SourceRange -> ParserErrorType -> ParserError
rangedError range = { range, toks: [], type: _ }