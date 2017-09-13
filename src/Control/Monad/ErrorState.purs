module Control.Monad.ErrorState where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Transformerless.State (State, runState)
import Control.Monad.Transformerless.State as State
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

newtype EState s e a = EState (State s (Either e a))

unwrap :: forall s e a. EState s e a -> State s (Either e a)
unwrap (EState s) = s

runEState :: forall s e a. s -> EState s e a -> Either e (Tuple s a)
runEState init (EState s) = case runState s init of
  Tuple (Left e) _ -> Left e
  Tuple (Right a) s -> Right (Tuple s a)

get :: forall s e. EState s e s
get = EState $ Right <$> State.get

put :: forall s e. s -> EState s e Unit
put s = EState do
  State.put s
  pure $ Right unit

gets :: forall s e a. (s -> a) -> EState s e a
gets f = EState do
  a <- State.gets f
  pure $ Right a

modify :: forall s e. (s -> s) -> EState s e Unit
modify f = EState do
  State.modify f
  pure $ Right unit

derive instance functorEState :: Functor (EState s e)

instance applyEState :: Apply (EState s e) where
  apply :: forall a b. EState s e (a -> b) -> EState s e a -> EState s e b
  apply (EState sf) (EState sa)= EState do
    f <- sf
    case f of
      Left e -> pure $ Left e
      Right f' -> do
        a <- sa
        case a of
          Left e' -> pure $ Left e'
          Right a' -> pure $ Right $ f' a'

instance applicativeEState :: Applicative (EState s e) where
  pure a = EState $ pure $ Right a

instance bindEState :: Bind (EState s e) where
  bind :: forall a b. EState s e a -> (a -> EState s e b) -> EState s e b
  bind (EState sa) k = EState do
    a <- sa
    case a of
      Left e -> pure $ Left e
      Right a' -> unwrap (k a')

instance monadEState :: Monad (EState s e)

instance monadThrow :: MonadThrow e (EState s e) where
  throwError e = EState $ pure $ Left e

instance monadCatch :: MonadError e (EState s e) where
  catchError (EState sa) f = EState do
    a <- sa
    case a of
      Left err -> unwrap (f err)
      Right a -> pure $ Right a
