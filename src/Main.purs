module Main where

import Prelude
import Eval (primitiveFuncs, runComputations)
import REPL (ReplEff, runRepl)
import Parser (readFile)
import Macro (macroize)
import CodeGen.JS.Compile (compile)
import CodeGen.JS.JS (generateJS)

import Control.Monad.Eff.Console as Console
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Trampoline (runTrampoline)

import Data.Array ((!!), drop)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Data.String (joinWith)
import Data.Foldable (intercalate)
import Data.Traversable (traverse)

import Node.Process (PROCESS, argv)

repl :: forall e. Eff (ReplEff e) Unit
repl = runAff (Console.log <<< message) pure runRepl

runOne :: forall e. String -> Eff ( console :: Console.CONSOLE | e ) Unit
runOne input = case runTrampoline $ runComputations primitiveFuncs input of
  Left err -> Console.error $ show err
  Right (Tuple _ wff) -> Console.print wff

codeGen :: forall e. String -> Eff ( console :: Console.CONSOLE | e ) Unit
codeGen input = case readFile input of
  Left err -> Console.error $ show err
  Right wffs -> case traverse compile $ macroize wffs of
    Left err -> Console.error $ show err
    Right jsexprs ->
      let output = generateJS true 0 <$> jsexprs
       in Console.log $ intercalate "\n" output

main :: Eff (ReplEff (process :: PROCESS)) Unit
main = do
  args <- drop 2 <$> argv
  case args !! 0 of
       Just "-i" -> repl
       Just "--interactive" -> repl
       Just "-c" -> codeGen $ joinWith "\n" $ drop 1 args
       Just "--compile" -> codeGen $ joinWith "\n" $ drop 1 args
       Just _ -> runOne $ joinWith "\n" $ drop 1 args
       _ -> Console.error "Please provide a valid command"
