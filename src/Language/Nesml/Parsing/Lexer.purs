module Language.Nesml.Parsing.Lexer where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get, modify, modify_)
import Data.Array (elem)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (binary, decimal, hexadecimal)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.String as Str
import Data.String.CodePoints as StrP
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Language.Nesml.Parsing.Errors (ParserError, ParserErrorType(..), positionedError, rangedError)
import Language.Nesml.Parsing.Monad (ParserM, runParserM)
import Language.Nesml.Parsing.Position (applyDelta, codePointDelta, posDelta, tokenDelta)
import Language.Nesml.Parsing.Types (SourcePos, SourceToken, Token(..))
import Language.Nesml.Parsing.Utils (internalError, is, isDigit, isIdentifierChar, isIdentifierHeadChar, isLetter, isOperatorChar, isUpperLetter, operatorString, reservedOperators)

type LexerState =
  { src :: String
  , pos :: SourcePos
  , code :: StateCode
  , strbuf :: String
  }

init :: String -> LexerState
init src =
  { src
  , pos: { ln: 1, col: 1 }
  , code: Initial
  , strbuf: ""
  }

data StateCode
  = Initial
  | InComment Int

derive instance Eq StateCode
derive instance Ord StateCode
derive instance Generic StateCode _
instance Show StateCode where
  show = genericShow

inComment :: StateCode -> Boolean
inComment = case _ of
  InComment _ -> true
  _ -> false

type Lexer a = ParserM ParserError LexerState a

pop :: Lexer (Maybe CodePoint)
pop = do
  { src } <- get
  case Str.uncons src of
    Nothing -> pure Nothing
    Just { head: cp, tail: rest } -> do
      modify_ $ \s ->
        ( s
            { src = rest
            , pos = applyDelta s.pos (codePointDelta cp)
            }
        )

      pure $ Just cp

popWhile :: (CodePoint -> Boolean) -> Lexer String
popWhile pred = go List.Nil
  where
  go cps = do
    peekCodePoint >>= case _ of
      Just cp
        | pred cp -> pop *> go (cp : cps)
      _ -> pure $ Str.fromCodePointArray <<< Array.fromFoldable <<< List.reverse $ cps

popString :: String -> Lexer (Maybe String)
popString str = do
  let size = StrP.length str
  peek size >>= case _ of
    Just str'
      | str == str' -> do
          Just str <$ (replicateA size pop :: Lexer (List _))
    _ -> pure Nothing

peekCodePoint :: Lexer (Maybe CodePoint)
peekCodePoint = do
  { src } <- get
  pure $ _.head <$> Str.uncons src

peek :: Int -> Lexer (Maybe String)
peek size = do
  get <#> case _ of
    { src }
      | Str.length src >= size -> Just $ Str.take size src
      | otherwise -> Nothing

sendBuf :: Lexer Unit
sendBuf = do
  pop >>= case _ of
    Just cp -> modify_ $ \s -> s { strbuf = s.strbuf <> (StrP.singleton cp) }
    Nothing -> pure unit

enterComment :: Lexer Unit
enterComment = do
  modify_ $ \s@{ code } -> case code of
    InComment depth -> s { code = InComment (depth + 1) }
    _ -> s { code = InComment 1 }

leaveComment :: Lexer (Maybe String)
leaveComment = do
  { code, strbuf } <- get
  case code of
    InComment depth
      | depth > 1 -> do
          Nothing <$ modify (_ { code = InComment (depth - 1) })
      | otherwise -> do
          Just strbuf <$ modify (_ { code = Initial, strbuf = "" })
    _ -> liftEffect $ throw "Invalid call to `leaveComment` outside comment"

type LexResult = Either (LexerState /\ ParserError) SourceToken

lex :: String -> Effect (Array LexResult)
lex src = go Nil $ { src, pos: { ln: 1, col: 1 }, code: Initial, strbuf: "" }
  where
  go results st = do
    st' /\ res <- runParserM st (skipWhitespacesComments *> token)
    case res of
      Left e -> go (Left (st' /\ e) : results) st'
      Right tok
        | tok.token == TokEOF -> pure $ Array.fromFoldable <<< List.reverse $ Right tok : results
        | otherwise -> go (Right tok : results) st'

token :: Lexer SourceToken
token = do
  { pos } <- get

  tok <- pop >>= case _ of
    Nothing -> pure TokEOF
    Just cp
      | cp `is` ')' -> pure TokRightParens
      | cp `is` '{' -> pure TokLeftBrace
      | cp `is` '}' -> pure TokRightBrace
      | cp `is` '[' -> pure TokLeftSquare
      | cp `is` ']' -> pure TokRightSquare
      | cp `is` ',' -> pure TokComma
      | cp `is` ';' -> pure TokSemicolon
      | cp `is` '`' -> pure TokBacktick
      | cp `is` '(' -> tokLeftParens
      | cp `is` '0' -> tokInteger
      | isDigit cp -> tokDecimalInteger "" cp
      | isUpperLetter cp -> tokUpperIdentifier Nil cp
      | isIdentifierHeadChar cp -> tokLowerIdentifier Nil cp
      | isOperatorChar cp -> tokOperator [] false cp
      | otherwise -> throwError $ rangedError { start: pos, end: applyDelta pos (posDelta 0 1) } (UnexpectedToken $ StrP.singleton cp)

  pure $ tok `annotateAt` pos

  where
  tokInteger = do
    start <- flip applyDelta (posDelta 0 (-1)) <<< _.pos <$> get
    peekCodePoint >>= case _ of
      Just cp
        | cp `is` 'x' -> do
            rest <- pop *> popWhile (\c -> c `elem` (StrP.toCodePointArray "0123456789abcdefABCDEF"))
            let input = "0" <> StrP.singleton cp <> rest
            if input == "0x" then throwError $ rangedError { start, end: applyDelta start (posDelta 0 2) } $ UnexpectedToken "0x"
            else case Int.fromStringAs hexadecimal rest of
              Nothing -> internalError $ "Failed to decode as a hexadecimal integer: " <> input
              Just i -> pure $ TokInteger input i
        | cp `is` 'b' -> do
            rest <- pop *> popWhile (\c -> c `elem` (StrP.toCodePointArray "01_"))
            let input = "0" <> StrP.singleton cp <> rest
            if input == "0b" then throwError
              $ rangedError { start, end: applyDelta start (posDelta 0 2) }
              $ UnexpectedToken "0b"
            else case Int.fromStringAs binary $ Str.replaceAll (Str.Pattern "_") (Str.Replacement "") rest of
              Nothing -> internalError $ "Failed to decode as a binary integer: " <> input
              Just i -> pure $ TokInteger input i
        | cp `elem` (StrP.toCodePointArray "0123456789") -> tokDecimalInteger "" (StrP.codePointFromChar '0')
      _ -> pure $ TokInteger "0" 0

  tokDecimalInteger sign cp = do
    rest <- popWhile isDigit
    let inp = sign <> StrP.singleton cp <> rest
    case Int.fromStringAs decimal inp of
      Just i -> pure $ TokInteger inp i
      Nothing -> internalError $ "Failed to decode as a decimal integer: " <> inp

  tokUpperIdentifier qual cp = do
    start <- _.pos <$> get
    rest <- popWhile isIdentifierChar
    let
      inp = StrP.singleton cp <> rest
      isn'tModuleNameLike = Array.any (not <<< isLetter) (StrP.toCodePointArray inp)
    if isn'tModuleNameLike then
      do
        peek 2 >>= case _ of
          Just "::" -> do
            end <- _.pos <$> get
            throwError $ rangedError { start, end } InvalidNamespaceCharacter
          _ -> pure $ TokUpperIdentifier (Array.fromFoldable <<< List.reverse $ qual) inp
    else do
      popString "::" >>= case _ of
        Nothing -> pure $ TokUpperIdentifier (Array.fromFoldable <<< List.reverse $ qual) inp
        Just _ -> do
          pop >>= case _ of
            Just cp'
              | isUpperLetter cp' -> tokUpperIdentifier (inp : qual) cp'
              | isIdentifierHeadChar cp' -> tokLowerIdentifier (inp : qual) cp'
              | isOperatorChar cp' -> tokOperator (Array.fromFoldable $ List.reverse (inp : qual)) false cp'
              | cp' `is` '(' -> pop >>= maybe
                  (get <#> _.pos >>= \pos -> throwError $ positionedError pos UnexpectedEOF)
                  (tokOperator (Array.fromFoldable $ List.reverse (inp : qual)) true)
              | otherwise -> do
                  pos <- _.pos <$> get
                  throwError $ positionedError pos (UnexpectedToken $ StrP.singleton cp')
            Nothing -> do
              end <- _.pos <$> get
              throwError $
                rangedError { start: applyDelta end (posDelta 0 (-2)), end } (UnexpectedToken "::")

  tokLowerIdentifier qual cp = do
    start <- flip applyDelta (posDelta 0 (-1)) <<< _.pos <$> get
    rest <- popWhile isIdentifierChar
    let inp = StrP.singleton cp <> rest
    case inp of
      "_"
        | qual /= Nil -> do
            end <- _.pos <$> get
            throwError $ rangedError { start, end } QualifiedWildcard
        | otherwise -> pure $ TokUnderscore
      _ -> pure $ TokLowerIdentifier (Array.fromFoldable <<< List.reverse $ qual) inp

  tokLeftParens = do
    start <- _.pos <$> get
    peekCodePoint >>= case _ of
      Just cp
        | isOperatorChar cp -> do
            void pop
            tok <- tokOperator [] true cp
            peekCodePoint >>= case _ of
              Just rp
                | rp `is` ')' -> pure tok
              _ -> do
                end <- _.pos <$> get
                throwError $
                  rangedError { start, end } (UnexpectedToken (fromMaybe "<unexpected>" $ operatorString tok))
      _ -> pure TokLeftParens

  tokOperator qual parens ch = do
    start <- _.pos <$> get

    when (not $ isOperatorChar ch) do
      throwError
        $ rangedError { start, end: applyDelta start (posDelta 0 1) }
        $ UnexpectedToken (StrP.singleton ch)

    rest <- popWhile isOperatorChar
    let input = StrP.singleton ch <> rest
    case input of
      "-" -> do
        peekCodePoint >>= case _ of
          Just cp
            | isDigit cp
            , Array.null qual -> pop *> tokDecimalInteger "-" cp
          _ -> pure $ TokOperator qual parens "-"
      "+" -> do
        peekCodePoint >>= case _ of
          Just cp
            | isDigit cp
            , Array.null qual -> pop *> tokDecimalInteger "+" cp
          _ -> pure $ TokOperator qual parens "-"
      "=" | Array.null qual -> pure TokEqual
      "->" | Array.null qual -> pure TokRightArrow
      _
        | not $ input `Set.member` reservedOperators -> pure $ TokOperator qual parens input
        | otherwise -> do
            end <- _.pos <$> get
            throwError $ rangedError { start, end } ReservedSymbolsOperator

  annotateAt tok pos =
    { range: { start: pos, end: applyDelta pos (tokenDelta tok) }
    , token: tok
    }

skipWhitespacesComments :: Lexer Unit
skipWhitespacesComments = do
  com <- comment
  ws <- whitespaces
  case com, ws of
    Nothing, Nothing -> pure unit
    _, _ -> skipWhitespacesComments

whitespaces :: Lexer (Maybe String)
whitespaces = do
  { code, strbuf } <- get
  peekCodePoint >>= case _ of
    Just cp
      | not (inComment code)
      , cp `elem` StrP.toCodePointArray "\t\r\n " -> do
          sendBuf
          whitespaces
    _ -> do
      modify (_ { strbuf = "" })
        $> if strbuf == "" then Nothing else Just strbuf

comment :: Lexer (Maybe String)
comment = do
  { code, pos } <- get
  peek 2 >>= case _ of
    Just "{*" -> do
      sendBuf
      sendBuf
      enterComment
      comment
    Just "*}"
      | not (inComment code) -> do
          void $ (replicateA 2 pop :: Lexer (List _))
          throwError $ rangedError { start: pos, end: applyDelta pos (posDelta 0 2) } UnexpectedCloseComment
      | otherwise -> do
          sendBuf
          sendBuf
          leaveComment >>= case _ of
            Just com -> pure $ Just com
            Nothing -> comment
    _ -> peekCodePoint >>= case _ of
      Nothing
        | inComment code -> do
            modify_ $ \s -> s { strbuf = "", code = Initial }
            throwError $ positionedError pos MissingCloseComment
      Just _
        | inComment code -> do
            sendBuf
            comment
      _ -> pure Nothing
