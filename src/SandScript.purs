module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import SandScript.Eval (eval)
import SandScript.Parser (readExpr)
import SandScript.Errors (extractValue, trapError, runEffThrows, liftThrows)
import SandScript.Types
import SandScript.Variables (nullEnv)

import Node.ReadLine

evalString :: forall r. Env -> String -> REff r String
evalString env expr = runEffThrows $ liftA1 show $ (liftThrows $ readExpr expr) >>= eval env

main = do
  interface <- createInterface noCompletion

  let lineHandler :: forall r. Env -> String -> REff (console :: CONSOLE | r) Interface
      lineHandler env args = do
        evaled <- evalString env args
        log evaled
        prompt interface

  setPrompt "> " 2 interface
  prompt interface
  n <- nullEnv
  setLineHandler (lineHandler n) interface
