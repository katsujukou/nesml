module Language.Nesml.Parsing.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type SourcePos =
  { ln :: Int
  , col :: Int
  }

type SourceRange =
  { start :: SourcePos
  , end :: SourcePos
  }

data Token
  = TokLeftParens
  | TokRightParens
  | TokLeftBrace
  | TokRightBrace
  | TokLeftSquare
  | TokRightSquare
  | TokComma
  | TokSemicolon
  | TokBacktick
  | TokUnderscore
  | TokEqual
  | TokRightArrow
  | TokOperator (Array String) Boolean String
  | TokLowerIdentifier (Array String) String
  | TokUpperIdentifier (Array String) String
  | TokInteger String Int
  | TokEOF

derive instance Eq Token
derive instance Ord Token
derive instance Generic Token _
instance Show Token where
  show = genericShow

type SourceToken =
  { range :: SourceRange
  , token :: Token
  }