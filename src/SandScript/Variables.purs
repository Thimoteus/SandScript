module SandScript.Variables where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Array hiding (cons)
import Data.Traversable

import Control.Apply ((*>))
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Class
import Control.Monad.Error.Trans
import Control.Monad.Error.Class

import SandScript.Types
import SandScript.Util
import SandScript.Eval.Primitives

nullEnv :: LispF Env
nullEnv = newRef []

isBound :: Env -> String -> LispF Boolean
isBound envRef var = readRef envRef >>= return <<< maybe false (const true) <<< lookup var

getVar :: Env -> String -> EffThrowsError LispVal
getVar envRef var = do
  env <- liftEff $ readRef envRef
  maybe (throwError $ UnboundedVar "Getting an unbound variable" var)
        (liftEff <<< readRef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> EffThrowsError LispVal
setVar envRef var value = do
  env <- liftEff $ readRef envRef
  maybe (throwError $ UnboundedVar "Setting an unbound variable" var)
        (liftEff <<< (flip writeRef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> EffThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftEff $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value *> return value
    else do
      valueRef <- liftEff $ newRef value
      env <- liftEff $ readRef envRef
      liftEff $ writeRef envRef ((Tuple var valueRef) : env)
      return value

bindVars :: Env -> Array (Tuple String LispVal) -> LispF Env
bindVars envRef bindings = readRef envRef >>= extendEnv bindings >>= newRef where
  extendEnv bindings env = map (++ env) (traverse addBindings bindings)
  addBindings (Tuple var value) = do
    ref <- newRef value
    return (Tuple var ref)
