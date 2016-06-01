module Env where

import Prelude
import Syntax (WFF, ThrowsError, LangError(SetImmutable, UnboundVar))

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State.Trans (StateT, evalStateT, put, get)

import Data.StrMap as Map
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple)

type Env = Map.StrMap WFF
type MThrowsError m = ExceptT LangError m
type State m a = StateT Env (MThrowsError m) a
{-- type State m a = (MonadState Env m, MonadError LangError m) => m a --}

{-- isBound :: forall m. Monad m => String -> State m Boolean --}
{-- isBound k = pure <<< Map.member k =<< (get :: State m Env) --}

getVar :: forall m. Monad m => String -> State m WFF
getVar var = do
  env <- get
  case Map.lookup var env of
       Just x -> pure x
       _ -> throwError $ UnboundVar "Definition does not exist for" var

defineVar :: forall m. Monad m => String -> WFF -> State m WFF
defineVar key val = do
  env <- get
  if Map.member key env
     then throwError $ SetImmutable key
     else do
       put $ Map.insert key val env
       pure val

bindVars :: List (Tuple String WFF) -> Env -> Env
bindVars xs = Map.union $ Map.fromList xs

retainState :: forall m a. Monad m => State m a -> State m a
retainState sa = do
  env :: Env <- get
  a <- sa
  put env
  pure a

liftThrows :: forall a m e. MonadError e m => Either e a -> m a
liftThrows (Right v) = pure v
liftThrows (Left e) = throwError e

liftState :: forall m a. Monad m => ThrowsError a -> State m a
liftState (Right v) = pure v
liftState (Left e) = throwError e

runState :: forall a m. Monad m => Env -> State m a -> m (ThrowsError a)
runState e s = runExceptT $ evalStateT s e
