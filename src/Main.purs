module Main where

import Prelude

import SandScript.Parser (read)
import SandScript.Eval (eval)
import SandScript.Errors (ThrowsError, extractValue, trapError)

import Control.Apply ((*>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Node.ReadLine as RL

type Repl a = Eff ( console :: CONSOLE,  readline :: RL.READLINE, err :: EXCEPTION ) a

replInterface :: Repl RL.Interface
replInterface = RL.createConsoleInterface RL.noCompletion

repl :: String -> Repl Unit
repl s = do
  let evaled :: ThrowsError String
      evaled = show <$> (read s >>= eval)
      trapped :: ThrowsError String
      trapped = trapError evaled
      extracted :: String
      extracted = extractValue trapped
  log extracted

main :: Repl Unit
main = do
   interface <- replInterface
   RL.setPrompt ">> " 2 interface
   RL.setLineHandler interface (handler interface)
   RL.prompt interface

handler :: RL.Interface -> String -> Repl Unit
handler i =
  case _ of
       "exit" -> RL.close i
       x -> repl x *> RL.prompt i

