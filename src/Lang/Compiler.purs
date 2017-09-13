module Lang.Compiler where

import Prelude

import Data.StrMap as Map
import Lang.AST as AST
import Lang.JavaScript as JS
import Matryoshka (cata)

compile :: AST.Expr -> JS.JSExpr
compile = cata compile' where
  compile' :: AST.ExprF JS.JSExpr -> JS.JSExpr
  compile' (AST.Lit l) = JS.jslit $ transformLit l
  compile' (AST.Ident i) = JS.jsIdent i
  compile' (AST.Array xs) = JS.jsArray xs
  compile' (AST.Lambda arg body) = JS.jsLambda arg body
  compile' (AST.LetIn name e expr) =
    JS.jsIIFE (Map.singleton name e) expr
  compile' (AST.Apply f x) = JS.jsApply f x

transformLit :: AST.Literal -> JS.JSLit
transformLit = case _ of
  AST.String s -> JS.JSString s
  AST.Int n -> JS.JSInt n
  AST.Number n -> JS.JSNum n
  AST.Bool b -> JS.JSBool b
  AST.Char c -> JS.JSChar c
