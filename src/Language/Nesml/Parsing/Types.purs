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
  | TokEOF

derive instance Eq Token
derive instance Ord Token
derive instance Generic Token _
instance Show Token where
  show = genericShow

data Comment l
  = Comment String
  | Line l
  | Space Int

derive instance Eq l => Eq (Comment l)
derive instance Ord l => Ord (Comment l)
derive instance Generic (Comment l) _
instance Show l => Show (Comment l) where
  show = genericShow

data LineFeed = LF | CR

derive instance Eq LineFeed
derive instance Ord LineFeed
derive instance Generic LineFeed _
instance Show LineFeed where
  show LF = "LF"
  show CR = "CR"