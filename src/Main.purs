module Main where

import Prelude

import SandScript.Env (Env, State, initEnv)
import SandScript.REPL (Command(..), SSCiDirective(..), runComputation, sandScript, match, help)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log, print)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

import Node.ReadLine as RL

type Effects = ( console :: Console.CONSOLE, readline :: RL.READLINE, err :: EXCEPTION )
type IO a = State (Aff Effects) a

type CompletionRec = { completions :: Array String, matched :: String }
type Interface = RL.Interface

createInterface :: Aff Effects Interface
createInterface = liftEff (RL.createConsoleInterface RL.noCompletion)

createInterfaceIO :: IO Interface
createInterfaceIO = liftAff createInterface

prompt :: Interface -> Aff Effects Unit
prompt i = liftEff $ RL.prompt i

promptIO :: Interface -> IO Unit
promptIO = liftAff <<< prompt

setPrompt :: String -> Int -> Interface -> Aff Effects Unit
setPrompt s n i = liftEff $ RL.setPrompt s n i

setPromptIO :: String -> Int -> Interface -> IO Unit
setPromptIO s n i = liftAff $ setPrompt s n i

close :: Interface -> Aff Effects Unit
close i = liftEff $ RL.close i

closeIO :: Interface -> IO Unit
closeIO = liftAff <<< close

readLine :: Interface -> Aff Effects String
readLine i = makeAff $ const $ RL.setLineHandler i

runRepl :: Aff Effects Unit
runRepl = do
  interface <- createInterface
  log sandScript
  setPrompt ">> " 2 interface
  prompt interface
  loop initEnv interface

loop :: Env -> Interface -> Aff Effects Unit
loop env int = do
  prompt int
  input <- readLine int
  case match input of
       Directive Quit -> close int
       Directive Help -> do
         log help
         loop env int
       Directive (UnknownDirective d) -> do
         log $ "Unknown directive: " <> d
         loop env int
       Command x -> do
         result <- runComputation env x
         case result of
              Left err -> print err
              Right (Tuple e wff) -> do
                print wff
                loop e int

main :: Eff Effects Unit
main = runAff (Console.log <<< message) pure runRepl
