module Language.Nesml.Parsing.Utils where

import Prelude

import Data.Array (elem)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.String (CodePoint)
import Data.String.CodePoints as StrP
import Data.Tuple.Nested (type (/\), (/\))

span :: (CodePoint -> Boolean) -> String -> String /\ String
span p = go ""
  where
  go i f = case Str.uncons f of
    Nothing -> i /\ f
    Just { head, tail } ->
      if p head then go (i <> Str.singleton head) tail
      else i /\ f

isDigit :: CodePoint -> Boolean
isDigit = (_ `elem` (StrP.toCodePointArray "0123456789"))

isWhitespace :: CodePoint -> Boolean
isWhitespace = (_ `elem` (StrP.toCodePointArray " \t\r\n"))
