module SandScript.Variables where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Array
import Data.Traversable

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Class
import Control.Monad.Error.Trans
import Control.Monad.Error.Class

import SandScript.Types
import SandScript.Util

nullEnv :: forall r. Eff (ref :: REF | r) Env
nullEnv = newRef []

isBound :: forall r. Env -> String -> REff r Boolean
isBound envRef var = readRef envRef >>= return <<< maybe false (const true) <<< lookup var

getVar :: forall r. Env -> String -> EffThrowsError r LispVal
getVar envRef var = do
  env <- liftEff $ readRef envRef
  maybe (throwError $ UnboundedVar "Getting an unbound variable" var)
        (liftEff <<< readRef)
        (lookup var env)

setVar :: forall r. Env -> String -> LispVal -> EffThrowsError r LispVal
setVar envRef var value = do
  env <- liftEff $ readRef envRef
  maybe (throwError $ UnboundedVar "Setting an unbound variable" var)
        (liftEff <<< (flip writeRef value))
        (lookup var env)
  return value

defineVar :: forall r. Env -> String -> LispVal -> EffThrowsError r LispVal
defineVar envRef var value = do
  alreadyDefined <- liftEff $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else do
      valueRef <- liftEff $ newRef value
      env <- liftEff $ readRef envRef
      liftEff $ writeRef envRef ((Tuple var valueRef) : env)
      return value

bindVars :: forall r. Env -> Array (Tuple String LispVal) -> REff r Env
bindVars envRef bindings = readRef envRef >>= extendEnv bindings >>= newRef where
  extendEnv bindings env = map (++ env) (traverse addBindings bindings)
  addBindings (Tuple var value) = do
    ref <- newRef value
    return (Tuple var ref)
