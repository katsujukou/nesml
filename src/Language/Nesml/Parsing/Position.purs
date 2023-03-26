module Language.Nesml.Parsing.Position
  ( PosDelta
  , applyDelta
  , codePointDelta
  , posDelta
  , textDelta
  , tokenDelta
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.String (CodePoint, length)
import Data.String as Str
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Language.Nesml.Parsing.Types (SourcePos, Token(..))
import Language.Nesml.Parsing.Utils (is)
import Record as R
import Type.Proxy (Proxy(..))

{-- In order to represent delta of '\r' (carｒige return) as `(0 /\ -infinity)`, 
we encode delta of column as `Number`, not `Int`. 　--}
newtype PosDelta = PosDelta (Tuple Int Number)

posDelta :: Int -> Int -> PosDelta
posDelta dl dc = PosDelta $ Tuple dl (Int.toNumber dc)

applyDelta :: SourcePos -> PosDelta -> SourcePos
applyDelta p (PosDelta (0 /\ dc)) = p # R.modify (Proxy :: _ "col") applyDeltaCol
  where
  applyDeltaCol col = max 1 (Int.ceil $ (Int.toNumber col + dc))
applyDelta p (PosDelta (l /\ c)) = applyDelta (p { ln = 1 + p.ln, col = 1 }) $ PosDelta ((l - 1) /\ c)

textDelta :: String -> Int /\ Int
textDelta = lines >>> go 0
  where
  go dl = Array.uncons >>> case _ of
    Nothing -> 0 /\ 0
    Just { head: h, tail: t }
      | Array.null t -> dl /\ length h
      | otherwise -> go (1 + dl) t

codePointDelta :: CodePoint -> PosDelta
codePointDelta = case _ of
  cp
    | cp `is` '\n' -> PosDelta $ 1 /\ 0.0
    | cp `is` '\r' -> PosDelta $ 0 /\ (-infinity)
    | otherwise -> PosDelta $ 0 /\ 1.0

tokenDelta :: Token -> PosDelta
tokenDelta = PosDelta <<< case _ of
  TokLeftParens -> (0 /\ 1.0)
  TokRightParens -> (0 /\ 1.0)
  TokLeftBrace -> (0 /\ 1.0)
  TokRightBrace -> (0 /\ 1.0)
  TokLeftSquare -> (0 /\ 1.0)
  TokRightSquare -> (0 /\ 1.0)
  TokRightArrow -> (0 /\ 2.0)
  TokComma -> (0 /\ 1.0)
  TokSemicolon -> (0 /\ 1.0)
  TokBacktick -> (0 /\ 1.0)
  TokEqual -> (0 /\ 1.0)
  TokUnderscore -> (0 /\ 1.0)
  TokOperator prefs inParens op ->
    let
      dc = (Int.toNumber $ Str.length op)
        + (if inParens then 2.0 else 0.0)
        + foldl (\acc -> (1.0 + acc + _) <<< Int.toNumber <<< Str.length) 0.0 prefs
    in
      (0 /\ dc)
  TokLowerIdentifier qual name -> identDelta qual name
  TokUpperIdentifier qual name -> identDelta qual name
  TokInteger raw _ -> (0 /\ (Int.toNumber <<< Str.length $ raw))
  TokEOF -> (0 /\ 0.0)

  where
  identDelta qual name =
    let
      dc = (Int.toNumber $ Str.length name) + (foldl (+) 0.0 $ map ((1.0 + _) <<< Int.toNumber <<< Str.length) qual)
    in
      0 /\ dc