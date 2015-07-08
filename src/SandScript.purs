module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Console

import SandScript.Eval (eval)
import SandScript.Parser (readExpr)
import SandScript.Errors (extractValue, trapError)

import Node.ReadLine

main = do
  interface <- createInterface noCompletion

  let lineHandler args = do
        evaled <- return $ liftA1 show $ readExpr args >>= eval 
        log $ extractValue $ trapError evaled
        prompt interface

  setPrompt "> " 2 interface
  prompt interface
  setLineHandler lineHandler interface
