module SandScript.Env where

import Prelude

import SandScript.AST (WFF)
import SandScript.Errors (LangError(..), ThrowsError, extractValue)

import Control.Bind ((=<<))
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT, mapExceptT)
import Control.Monad.State.Trans (StateT, evalStateT, mapStateT, get, put)

import Data.Either (Either(..))
import Data.StrMap as Map
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple)
import Data.List (List)

type Env = Map.StrMap WFF

type MThrowsError m = ExceptT LangError m
type State m a = StateT Env (MThrowsError m) a

initEnv :: Env
initEnv = Map.empty

isBound :: forall m. Monad m => String -> State m Boolean
isBound k = pure <<< Map.member k =<< (get :: State m Env)

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

bindVars :: forall m. Monad m => List (Tuple String WFF) -> State m Env
bindVars xs = do
  env <- get
  let env' = Map.union env $ Map.fromList xs
  put env'
  pure env'

liftThrows :: forall m a. Monad m => ThrowsError a -> MThrowsError m a
liftThrows (Right v) = pure v
liftThrows (Left err) = throwError err

liftState :: forall m a. Monad m => ThrowsError a -> State m a
liftState (Right v) = pure v
liftState (Left e) = throwError e

runMThrows :: forall m. Monad m => MThrowsError m String -> m String
runMThrows action = runExceptT (trap action) >>= pure <<< extractValue
  where
    trap a = catchError a ((pure <<< show) :: LangError -> MThrowsError m String)

extractString :: forall m. Monad m => Env -> State m String -> m String
extractString env state = runMThrows $ evalStateT state env

transformState :: forall a b m1 m2. (Monad m1, Monad m2) => (m1 (ThrowsError (Tuple a Env)) -> m2 (ThrowsError (Tuple b Env))) -> State m1 a -> State m2 b
transformState f = mapStateT (mapExceptT f)

stateToMThrows :: forall a m. Monad m => Env -> State m a -> MThrowsError m a
stateToMThrows e s = evalStateT s e

mThrowsToM :: forall a m. Monad m => MThrowsError m a -> m (ThrowsError a)
mThrowsToM = runExceptT

runState :: forall a m. Monad m => Env -> State m a -> m (ThrowsError a)
runState e s = mThrowsToM $ stateToMThrows e s
