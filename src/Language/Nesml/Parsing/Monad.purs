module Language.Nesml.Parsing.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError)
import Control.Monad.State (class MonadState, get, put)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.List (List(..), reverse, (:))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Uncurried.RWSET (RWSET, runRWSET, rwseT)

newtype ParserM e s a = ParserM
  (RWSET Unit Unit s e Effect a)

derive newtype instance Functor (ParserM e s)
derive newtype instance Apply (ParserM e s)
derive newtype instance Applicative (ParserM e s)
derive newtype instance Bind (ParserM e s)
derive newtype instance Monad (ParserM e s)
derive newtype instance MonadThrow e (ParserM e s)
derive newtype instance MonadError e (ParserM e s)
derive newtype instance MonadState s (ParserM e s)
derive newtype instance MonadEffect (ParserM e s)

mkParser :: forall e s a. (s -> Effect (s /\ Either e a)) -> ParserM e s a
mkParser p = ParserM go
  where
  go = rwseT $ \_ s -> do
    s' /\ res <- p s
    pure (s' /\ res /\ unit)

runParserM :: forall e s a. s -> ParserM e s a -> Effect (s /\ Either e a)
runParserM s (ParserM p) = do
  s' /\ res /\ _ <- runRWSET unit s p
  pure (s' /\ res)

parseWhile :: forall e s a. (a -> Boolean) -> ParserM e s a -> ParserM e s (Array a)
parseWhile pred p = go Nil
  where
  go res = do
    r <- p
    if pred r then go (r : res)
    else pure $ fromFoldable $ reverse res

revert :: forall e s a. ParserM e s a -> a -> ParserM e s a
revert p a = do
  st <- get
  p `catchError` \_ -> do
    put st
    pure a

