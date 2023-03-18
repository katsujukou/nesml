module Language.Nesml.Parsing.Lexer where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (codePointFromChar, null)
import Data.String as Str
import Data.String.CodePoints as StrP
import Data.String.Pattern as Pattern
import Data.Tuple.Nested ((/\))
import Language.Nesml.Parsing.Errors (ParserError, ParserErrorType(..), rangedError)
import Language.Nesml.Parsing.Monad (ParserM, mkParser, parseWhile)
import Language.Nesml.Parsing.Position (advancePos, textDelta)
import Language.Nesml.Parsing.Types (SourcePos)
import Language.Nesml.Parsing.Utils (isWhitespace, span)

type LexerState =
  { source :: String
  , current :: SourcePos
  , commentDepth :: Int
  , strBuffer :: String
  }

type Lexer a = ParserM ParserError LexerState a

whitespacesAndComments :: Lexer (Array Boolean)
whitespacesAndComments = parseWhile identity do
  w <- whitespaces
  c <- comments
  pure $ w || not (null c)

comments :: Lexer String
comments = mkParser $ go
  where
  go s@{ source: src } = case StrP.uncons src of
    Nothing
      | s.commentDepth > 0 ->
          let
            commentRange = { start: s.current, end: advancePos s.current (textDelta $ s.strBuffer <> "*)") }
          in
            s /\ (Left $ rangedError commentRange MissingCloseComment)
      | otherwise -> s /\ Right ""
    Just { head: c, tail: rest }
      -- Start comment characters ("(*")
      | c == codePointFromChar '('
      , Just { head: next, tail: src' } <- StrP.uncons rest
      , next == codePointFromChar '*' ->
          go $ s
            { commentDepth = s.commentDepth + 1
            , strBuffer = s.strBuffer <> "(*"
            , source = src'
            }
      -- Close comment characters ("*)")
      | c == codePointFromChar '*'
      , Just { head: next, tail: src' } <- StrP.uncons rest
      , next == codePointFromChar ')' -> do
          let commentEnd = advancePos s.current (textDelta $ s.strBuffer <> "*)")
          case s.commentDepth of
            0 -> s /\ (Left $ rangedError { start: s.current, end: commentEnd } UnexpectedCloseComment)
            1 ->
              let
                s' =
                  { source: src'
                  , current: commentEnd
                  , commentDepth: 0
                  , strBuffer: ""
                  }
              in
                s' /\ (Right $ s.strBuffer <> "*)")
            depth -> go $ s
              { source = src'
              , strBuffer = s.strBuffer <> "*)"
              , commentDepth = depth - 1
              }
      | s.commentDepth > 0 -> go $ s
          { source = rest
          , strBuffer = s.strBuffer <> StrP.singleton c
          }
      | otherwise -> s /\ (Right s.strBuffer)

whitespaces :: Lexer Boolean
whitespaces = mkParser k
  where
  k s@{ current, source } =
    let
      spaces /\ src' = span isWhitespace source
      spaces' = Str.replace (Pattern.Pattern "\r\n") (Pattern.Replacement "\n") spaces
      s' = s { source = src', current = advancePos current (textDelta spaces') }
    in
      s' /\ Right (s /= s')
