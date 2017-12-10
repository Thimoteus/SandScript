module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.ErrorState (runEState)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.StrMap as Map
import Data.Tuple (Tuple(..))
import Lang.AST (Expr, Literal(..), applyE, ident, lambda, letin, lit)
import Lang.TypeChecker (Γ(..), typeInference)
import Lang.TypeChecker.Types (showType)

test :: Expr -> Eff (console :: CONSOLE) Unit
test e = do
  res <- case runEState 0 $ typeInference (Γ Map.empty) e of
    Left err -> pure $ show err
    Right (Tuple _ t) -> pure $ showType t
  log res

e0 :: Expr
e0 = letin "id" (lambda "x" (ident "x")) (ident "id")

e1 :: Expr
e1 = letin "id" (lambda "x" (ident "x")) (applyE (ident "id") (ident "id"))

e2 :: Expr
e2 = letin "id" (lambda "x" (letin "y" (ident "x") (ident "y"))) (applyE (ident "id") (ident "id"))

e3 :: Expr
e3 = letin "id" (lambda "x" (letin "y" (ident "x") (ident "y"))) (applyE (applyE (ident "id") (ident "id")) (lit (Int 2)))

e4 :: Expr
e4 = letin "id" (lambda "x" (applyE (ident "x") (ident "x"))) (ident "id")

e5 :: Expr
e5 = lambda "m" (letin "y" (ident "m") (letin "x" (applyE (ident "y") (lit (Bool true))) (ident "x")))

e6 :: Expr
e6 = applyE (lit (Int 2)) (lit (Int 2))

main :: Eff (console :: CONSOLE) Unit
main = traverse_ test [e0, e1, e2, e3, e4, e5, e6]
