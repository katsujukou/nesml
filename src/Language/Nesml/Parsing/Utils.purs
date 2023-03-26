module Language.Nesml.Parsing.Utils
  ( (./\)
  , and
  , breakOn
  , internalError
  , is
  , isDigit
  , isDot
  , isIdentifierChar
  , isIdentifierHeadChar
  , isLetter
  , isLowerLetter
  , isOperatorChar
  , isUnqualifiedIdentifier
  , isUpperLetter
  , isWhitespace
  , operatorString
  , or
  , reservedOperators
  , span
  ) where

import Prelude

import Data.Array (elem)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String (CodePoint, codePointFromChar, toCodePointArray)
import Data.String as Str
import Data.String.CodePoints as StrP
import Data.String.Utils (startsWith)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Language.Nesml.Parsing.Types (Token(..))

span :: (CodePoint -> Boolean) -> String -> String /\ String
span p = go ""
  where
  go i f = case Str.uncons f of
    Nothing -> i /\ f
    Just { head, tail } ->
      if p head then go (i <> Str.singleton head) tail
      else i /\ f

breakOn :: String -> String -> String /\ String
breakOn ndl target = go "" target
  where
  go l r = case Str.uncons r of
    Nothing -> l /\ ""
    Just { head, tail }
      | startsWith ndl r -> l /\ r
      | otherwise -> go (l <> StrP.singleton head) tail

isWhitespace :: CodePoint -> Boolean
isWhitespace = (_ `elem` (StrP.toCodePointArray " \t\r\n"))

isOperatorChar :: CodePoint -> Boolean
isOperatorChar = (_ `elem` (StrP.toCodePointArray "-+.~$\\/:*&|%#!?@<=>"))

isLowerLetter :: CodePoint -> Boolean
isLowerLetter = (_ `elem` (StrP.toCodePointArray "abcdefghijklmnopqrstuvwxyz"))

reservedOperators :: Set.Set String
reservedOperators = Set.fromFoldable
  [ "->"
  , "="
  ]

isUpperLetter :: CodePoint -> Boolean
isUpperLetter = (_ `elem` (StrP.toCodePointArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

isLetter :: CodePoint -> Boolean
isLetter = \c -> isLowerLetter c || isUpperLetter c

isDigit :: CodePoint -> Boolean
isDigit = (_ `elem` (StrP.toCodePointArray "0123456789"))

isIdentifierHeadChar :: CodePoint -> Boolean
isIdentifierHeadChar = \c -> isLetter c || (c `is` '_')

isIdentifierChar :: CodePoint -> Boolean
isIdentifierChar ch = isIdentifierHeadChar ch || isDigit ch || ch `is` '\''

isUnqualifiedIdentifier :: String -> Boolean
isUnqualifiedIdentifier = toCodePointArray >>> Array.uncons >>> maybe false ((_.head >>> isIdentifierHeadChar) ./\ (_.tail >>> Array.all isIdentifierChar))

isDot :: CodePoint -> Boolean
isDot = (_ `is` '.')

is :: CodePoint -> Char -> Boolean
is cp ch = cp == codePointFromChar ch

or :: forall a. (a -> Boolean) -> (a -> Boolean) -> (a -> Boolean)
or p1 p2 = \a -> p1 a || p2 a

infixr 4 or as \/

and :: forall a. (a -> Boolean) -> (a -> Boolean) -> (a -> Boolean)
and p1 p2 = \a -> p1 a && p2 a

infixr 4 or as ./\

operatorString :: Token -> Maybe String
operatorString = case _ of
  TokOperator _ _ op -> Just op
  _ -> Nothing

internalError :: forall a m. MonadEffect m => String -> m a
internalError = liftEffect <<< throw