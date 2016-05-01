module Main where

import Prelude

import Control.Monad.Eff.Console as Console
import Control.Monad.Eff (Eff)

import Data.Either (Either(Right, Left))
import Data.Tuple (Tuple(Tuple))

import SandScript.Eval (primitiveFuncs, runComputations)

runOne :: forall e. String -> Eff ( console :: Console.CONSOLE | e ) Unit
runOne input = do
  res <- runComputations primitiveFuncs input
  case res of
       Left err -> Console.error $ show err
       Right (Tuple _ wff) -> Console.print wff

main :: Eff (console :: Console.CONSOLE ) Unit
main = pure unit
