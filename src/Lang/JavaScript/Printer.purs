module Lang.JavaScript.Printer where

import Prelude

import Control.Monad.Transformerless.State as State
import Data.Array as Array
import Data.Functor.Mu (unroll)
import Data.StrMap (StrMap)
import Data.StrMap as Map
import Lang.JavaScript (JSExpr, JSExprF(..), JSLit(JSChar, JSString, JSBool, JSInt, JSNum))
import PPrint (Printer, commitLine, execPrinter, fresh, indent, initState, mappend, parens, showAndAppend, unindent)

pprintJS :: JSExpr -> Printer Unit
pprintJS expr = case unroll expr of
  JSLit l -> printLit l
  JSIdent i -> mappend i
  JSObject o -> printObj o
  JSArray js -> printArr js
  JSLambda arg expr -> printLambda arg expr
  IfThenElse cond onTrue onFalse -> printIfThenElse cond onTrue onFalse
  JSBind name e -> printBind name e
  JSApply f x -> printApply f x
  IIFE bs ret -> printIIFE bs ret

printLit :: JSLit -> Printer Unit
printLit = case _ of
  JSNum n -> showAndAppend n
  JSInt n -> showAndAppend n
  JSBool b -> showAndAppend b
  JSString s -> showAndAppend s
  JSChar c -> showAndAppend c

printObj :: StrMap JSExpr -> Printer Unit
printObj o = do
  mappend "{"
  commitLine
  indent
  Map.foldM f unit o
  unindent
  mappend "}"
  where
    f :: Unit -> String -> JSExpr -> Printer Unit
    f _ key val = do
      mappend $ key <> ": "
      subexpr val
      mappend ","
      commitLine

printArr :: Array JSExpr -> Printer Unit
printArr arr = do
  mappend "["
  Array.foldM f unit arr
  mappend "]"
  where
    f :: Unit -> JSExpr -> Printer Unit
    f _ expr = do
      subexpr expr
      mappend ", "

printLambda :: String -> JSExpr -> Printer Unit
printLambda arg expr = do
  mappend $ parens arg
  mappend " => {"
  commitLine
  indent
  mappend "return "
  subexpr expr
  mappend ";"
  commitLine
  unindent
  mappend "}"

printIfThenElse :: JSExpr -> JSExpr -> JSExpr -> Printer Unit
printIfThenElse cond onTrue onFalse = do
  mappend "(function () {"
  commitLine
  indent
  v <- fresh
  printBind v cond
  mappend $ "if " <> parens v <> " {"
  commitLine
  do
    indent
    v' <- fresh
    printBind v' onTrue
    mappend $ "return " <> v' <> ";"
    commitLine
    unindent
  mappend "};"
  commitLine
  mappend "return "
  onF <- printLn onFalse
  mappend onF
  mappend ";"
  commitLine
  unindent
  mappend "})();"
  commitLine

printIIFE :: StrMap JSExpr -> JSExpr -> Printer Unit
printIIFE o e = do
  mappend "(function () {"
  commitLine
  indent
  Map.foldM (\ _ name expr -> printBind name expr) unit o
  e' <- printLn e
  mappend $ "return " <> e' <> ";"
  commitLine
  unindent
  mappend "})();"
  commitLine

printBind :: String -> JSExpr -> Printer Unit
printBind name expr = do
  mappend $ "const " <> name <> " = "
  subexpr expr
  mappend ";"
  commitLine

printApply :: JSExpr -> JSExpr -> Printer Unit
printApply f x = do
  f' <- printLn f
  mappend f'
  x' <- printLn x
  mappend (parens x')

-- used to "append" a subexpression's state to the parent's state
subexpr :: JSExpr -> Printer Unit
subexpr e = do
  s <- State.get
  State.put $ execPrinter s $ pprintJS e

printLn :: JSExpr -> Printer String
printLn e = pure $ _.line $ execPrinter initState $ pprintJS e
