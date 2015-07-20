module Main where

import Prelude

import Data.Array (drop)
import Data.List ((:), List(..), toList)
import Data.Array.Unsafe (unsafeIndex)
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Traversable (traverse)

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console

import SandScript.Eval (eval, primitiveBindings)
import SandScript.Parser (readExpr)
import SandScript.Errors (extractValue, trapError, runEffThrows, liftThrows)
import SandScript.Variables (bindVars)
import SandScript.Types
import SandScript.Util

import Node.ReadLine
import Node.Yargs
import Node.Yargs.Setup
import Node.Yargs.Applicative

evalString :: forall r. Env -> String -> LispF String
evalString env expr = runEffThrows $ liftA1 show $ (liftThrows $ readExpr expr) >>= eval env

loadLib :: Env -> String -> LispF Unit
loadLib env filename = (runEffThrows $ map show $ eval env (List (Atom "load" : String filename : Nil))) >>= error

runOne :: Array String -> LispF Unit
runOne args = do
  env <- primitiveBindings >>= (flip bindVars $ Cons ("args" & (List $ map String $ toList $ drop 1 args)) Nil)
  loadLib env (args `unsafeIndex` 0)

runRepl :: LispF Unit
runRepl = do
  interface <- createInterface noCompletion
  
  let lineHandler :: Env -> String -> LispF Interface
      lineHandler env args = do
        evaled <- evalString env args
        log evaled
        prompt interface

  initialEnvironment <- primitiveBindings
  loadLib initialEnvironment "lang/Prelude.sand"
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler (lineHandler initialEnvironment) interface
  return unit

app :: Array String -> LispF Unit
app ["repl"] = runRepl
app xs = runOne xs

main :: LispF Unit
main = do
  let setup = usage "$0 -r File" 
              <> example "$0 -r App.sand" "Run App.sand"
  runY setup $ app <$> yarg "r" ["run"] (Just "A filename") (Left ["repl"]) false
