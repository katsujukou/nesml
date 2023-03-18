module Language.Nesml.Parsing.Position where

import Prelude

import Data.String (length)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines)
import Data.Tuple.Nested (type (/\), (/\))
import Language.Nesml.Parsing.Types (SourcePos)
import Record as R
import Type.Proxy (Proxy(..))

type PosDelta = Int /\ Int

advancePos :: SourcePos -> PosDelta -> SourcePos
advancePos p (0 /\ c) = p # R.modify (Proxy :: _ "col") (_ + c) 
advancePos p (l /\ c) = advancePos (p { ln = 1 + p.ln, col = 1 }) $ (l - 1) /\ c

textDelta :: String -> Int /\ Int
textDelta = lines >>> go 0
  where
    go dl = Array.uncons >>> case _ of
      Nothing -> 0 /\ 0
      Just { head: h, tail: t }
        | Array.null t -> dl /\ length h
        | otherwise -> go (1 + dl) t