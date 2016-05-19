module SandScript.JS where

import Prelude

import Data.List as List
import Data.Array as Array
import Data.String (fromCharArray)
import Data.Foldable (class Foldable, intercalate)

type Name = String

data JSExpr = JSInt Int
            | JSBool Boolean
            | JSSymbol Name
            | JSReturn JSExpr
            | JSLambda (List.List Name) JSExpr
            | JSBinop JSBinop JSExpr JSExpr
            | JSAssign Name JSExpr
            | JSFunCall JSExpr (List.List JSExpr)

data JSBinop = Add | Sub | Mul | Div

indent :: Int -> String -> String
indent tabs e = (fromCharArray $ Array.replicate tabs ' ') <> e

unlines :: forall f. Foldable f => f String -> String
unlines = intercalate "\n"

generateJSOp :: JSBinop -> String
generateJSOp = case _ of
  Add -> " + "
  Sub -> " - "
  Mul -> "*"
  Div -> "/"

generateJS :: Boolean -> Int -> JSExpr -> String
generateJS doindent tabs = case _ of
  JSInt i -> show i
  JSBool b -> show b
  JSSymbol name -> name
  JSLambda vars expr -> (if doindent then indent tabs else id) $ unlines
    [ "function (" <> intercalate ", " vars <> ") {"
    , indent (tabs + 1) $ generateJS false (tabs + 1) expr ] <> indent tabs "}"
  JSBinop op e1 e2 -> "(" <> generateJS false tabs e1 <> " " <> generateJSOp op <> " " <> generateJS false tabs e2 <> ")"
  JSAssign var expr -> var <> " = " <> generateJS false tabs expr <> ";"
  JSFunCall f exprs -> "(" <> generateJS false tabs f <> ")(" <> intercalate ", " (generateJS false tabs <$> exprs) <> ")"
  JSReturn expr -> (if doindent then indent tabs else id) $ "return " <> generateJS false tabs expr <> ";"
