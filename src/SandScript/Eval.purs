module SandScript.Eval where

import Data.Maybe
import Data.Foldable
import Data.Tuple
import Data.Array (map)

import SandScript.Types
import SandScript.Util
import SandScript.Parser

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool false) ($ args) $ lookup func primitives

primitives :: Array (Tuple String (Array LispVal -> LispVal))
primitives = [ Tuple "+" (numericBinop (+))
             , Tuple "-" (numericBinop (-))
             , Tuple "*" (numericBinop (*))
             , Tuple "/" (numericBinop (/))
             , Tuple "mod" (numericBinop mod)
             , Tuple "symbol?" symbolChecker
             , Tuple "string?" stringChecker
             , Tuple "number?" numberChecker
             , Tuple "bool?" boolChecker
             , Tuple "symbol->string" sym2str
             , Tuple "string->symbol" str2sym ]

numericBinop :: (Number -> Number -> Number) -> Array LispVal -> LispVal
numericBinop op args = Number $ foldl1 op (map unpackNum args)

unpackNum :: LispVal -> Number
unpackNum (Number n) = n
unpackNum _ = 0

symbolChecker :: Array LispVal -> LispVal
symbolChecker [Atom _] = Bool true
symbolChecker _ = Bool false

stringChecker :: Array LispVal -> LispVal
stringChecker [String _] = Bool true
stringChecker _ = Bool false

numberChecker :: Array LispVal -> LispVal
numberChecker [Number _] = Bool true
numberChecker _ = Bool false

boolChecker :: Array LispVal -> LispVal
boolChecker [Bool _] = Bool true
boolChecker _ = Bool false

sym2str :: [LispVal] -> LispVal
sym2str [Atom n] = String $ show n
sym2str _ = String "ERROR"

str2sym :: [LispVal] -> LispVal
str2sym [String s] = Atom s
str2sym _ = String "ERROR"

rep :: String -> String
rep = show <<< eval <<< readExpr

