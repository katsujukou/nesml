module Language.Nesml.Parsing.Errors
  ( ParserError
  , ParserErrorType(..)
  , positionedError
  , rangedError
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Language.Nesml.Parsing.Types (SourceRange, Token, SourcePos)

data ParserErrorType
  = MissingCloseComment
  | MissingCloseDoubleQuote
  | QualifiedWildcard
  | InvalidNamespaceCharacter
  | UnexpectedCloseComment
  | UnexpectedChar CodePoint
  | UnexpectedToken String
  | UnexpectedEOF
  | ReservedSymbolsOperator
  | UnexpectedInput

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

positionedError :: SourcePos -> ParserErrorType -> ParserError
positionedError pos = rangedError { start: pos, end: pos }