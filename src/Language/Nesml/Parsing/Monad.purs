module Language.Nesml.Parsing.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (class MonadState)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.List (List(..), reverse, (:))
import Data.Tuple.Nested (type (/\), (/\))
import Uncurried.RWSE (RWSE, runRWSE, rwse)

newtype ParserM e s a = ParserM
  (RWSE Unit Unit s e a)

derive newtype instance Functor (ParserM e s)
derive newtype instance Apply (ParserM e s)
derive newtype instance Applicative (ParserM e s)
derive newtype instance Bind (ParserM e s)
derive newtype instance Monad (ParserM e s)
derive newtype instance MonadThrow e (ParserM e s)
derive newtype instance MonadError e (ParserM e s)
derive newtype instance MonadState s (ParserM e s)

mkParser :: forall e s a. (s -> s /\ Either e a) -> ParserM e s a
mkParser p = ParserM go 
  where
  go = rwse $ \_ s ->
    let s' /\ res = p s
    in  s' /\ res /\ unit

runParserM :: forall e s a. s -> ParserM e s a -> s /\ Either e a
runParserM s (ParserM p) = 
  let s' /\ res /\ _ = runRWSE unit s p
  in s' /\ res

parseWhile :: forall e s a. (a -> Boolean) -> ParserM e s a -> ParserM e s (Array a)
parseWhile pred p = go Nil
  where
    go res = do
      r <- p
      if pred r
      then go (r : res)
      else pure $ fromFoldable $ reverse res