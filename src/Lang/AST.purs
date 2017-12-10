module Lang.AST where

import Prelude

import Data.Functor.Mu (Mu(..))

data ExprF r
  = Lit Literal
  | Ident String
  | Array (Array r)
  | Lambda String r
  | LetIn String r r
  | Apply r r
  | Fix r
  | If r r r

derive instance eqExprF :: Eq r => Eq (ExprF r)
derive instance ordExprF :: Ord r => Ord (ExprF r)
derive instance functorExprF :: Functor ExprF

data Literal
  = String String
  | Int Int
  | Number Number
  | Bool Boolean
  | Char Char

derive instance eqLiteral :: Eq Literal
derive instance ordLiteral :: Ord Literal

type Expr = Mu ExprF

lit :: Literal -> Expr
lit = In <<< Lit

ident :: String -> Expr
ident = In <<< Ident

array :: Array Expr -> Expr
array = In <<< Array

letin :: String -> Expr -> Expr -> Expr
letin name bnd expr = In $ LetIn name bnd expr

lambda :: String -> Expr -> Expr
lambda name body = In $ Lambda name body

applyE :: Expr -> Expr -> Expr
applyE f expr = In $ Apply f expr

fix :: Expr -> Expr
fix = In <<< Fix

ifE :: Expr -> Expr -> Expr -> Expr
ifE c t f = In $ If c t f
