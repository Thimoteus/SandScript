module Main where

import Prelude

import Control.Monad.Eff.Console as Console
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (message)

import Data.Array ((!!), drop)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Data.String (joinWith)

import Node.Process (PROCESS, argv)

import SandScript.Eval (primitiveFuncs, runComputations)
import SandScript.REPL (ReplEff, runRepl)

repl :: forall e. Eff (ReplEff e) Unit
repl = runAff (Console.log <<< message) pure runRepl

runOne :: forall e. String -> Eff ( console :: Console.CONSOLE | e ) Unit
runOne input = do
  res <- runComputations primitiveFuncs input
  case res of
       Left err -> Console.error $ show err
       Right (Tuple _ wff) -> Console.print wff

main :: Eff (ReplEff (process :: PROCESS)) Unit
main = do
  args <- drop 2 <$> argv
  case args !! 0 of
       Just "-i" -> repl
       Just "--interactive" -> repl
       Just _ -> runOne $ joinWith "\n" args
       _ -> Console.error "Please provide a valid command"
