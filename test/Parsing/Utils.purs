module Test.Parsing.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Ext
import Language.Nesml.Parsing.Lexer (lex)
import Language.Nesml.Parsing.Types (SourcePos, SourceRange, SourceToken, Token)
import Test.Spec.Assertions (shouldEqual)

point :: SourcePos -> SourceRange
point p = { start: p, end: p }

ranged :: SourceRange -> Token -> SourceToken
ranged r tk = { range: r, token: tk }

posHead :: SourcePos
posHead = { ln: 1, col: 1 }

shouldBeParsedAs
  :: forall m
   . MonadEffect m
  => MonadThrow Ext.Error m
  => String
  -> Array Token
  -> m Unit
shouldBeParsedAs src toks = do
  res <- liftEffect (map (map _.token) <$> lex src)
  for_ (Array.zip res toks) \(tok1 /\ tok2) -> do
    tok1 `shouldEqual` Right tok2