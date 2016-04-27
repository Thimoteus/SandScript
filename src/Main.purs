module Main where

import Prelude

import SandScript.Parser (read)
import SandScript.Eval (eval)
import SandScript.Errors (ThrowsError, extractValue, trapError)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

repl :: forall eff. String -> Eff ( console :: CONSOLE | eff ) Unit
repl s = do
  let evaled :: ThrowsError String
      evaled = show <$> (read s >>= eval)
      trapped :: ThrowsError String
      trapped = trapError evaled
      extracted :: String
      extracted = extractValue trapped
  print extracted

main :: Eff ( console :: CONSOLE ) Unit
main = repl "(mod 30 9)"
