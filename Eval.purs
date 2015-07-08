module SandScript.Eval where

import Prelude 

import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Tuple
import Data.Array (uncons, length, head, tail, (:), zip)
import Data.Array.Unsafe (unsafeIndex)

import Control.Monad.Error.Class

import SandScript.Types
import SandScript.Errors
import SandScript.Util
import SandScript.Parser

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
       Bool false -> eval alt
       Bool true -> eval conseq
       notBool -> throwError $ TypeMismatch "bool" notBool
eval (List ls) = case uncons ls of
                      Just { head = Atom func, tail = args } -> traverse eval args >>= fapply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

fapply :: String -> Array LispVal -> ThrowsError LispVal
fapply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                         ($ args)
                         (lookup func primitives)

primitives :: Array (Tuple String (Array LispVal -> ThrowsError LispVal))
primitives = [ Tuple "+" (numericBinop (+))
             , Tuple "-" (numericBinop (-))
             , Tuple "*" (numericBinop (*))
             , Tuple "/" (numericBinop (/))
             , Tuple "mod" (numericBinop mod)
             , Tuple "symbol?" symbolChecker
             , Tuple "string?" stringChecker
             , Tuple "number?" numberChecker
             , Tuple "bool?" boolChecker
             -- N -> 2
             , Tuple "=" (numBoolBinop (==))
             , Tuple "<" (numBoolBinop (<))
             , Tuple ">" (numBoolBinop (>))
             , Tuple "!=" (numBoolBinop (/=))
             , Tuple ">=" (numBoolBinop (>=))
             , Tuple "<=" (numBoolBinop (<=))
             -- 2 -> 2
             , Tuple "&&" (boolBoolBinop (&&))
             , Tuple "||" (boolBoolBinop (||))
             -- String -> 2
             , Tuple "string=?" (strBoolBinop (==))
             , Tuple "string<?" (strBoolBinop (<))
             , Tuple "string>?" (strBoolBinop (>))
             , Tuple "string>=?" (strBoolBinop (>=))
             , Tuple "string<=?" (strBoolBinop (<=))
             -- list operators
             , Tuple "car" car
             , Tuple "cdr" cdr
             , Tuple "cons" cons
             , Tuple "eq?" eqv
             -- String -> String
             , Tuple "symbol->string" sym2str
             , Tuple "string->symbol" str2sym ]

numericBinop :: (Int -> Int -> Int) -> Array LispVal -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op val@[_] = throwError $ NumArgs 2 val
numericBinop op args = traverse unpackNum args >>= return <<< Number <<< foldl1 op

boolBinop :: forall a. (LispVal -> ThrowsError a) -> (a -> a -> Boolean) -> Array LispVal -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                               left <- unpacker $ args `unsafeIndex` 0
                               right <- unpacker $ args `unsafeIndex` 1
                               return $ Bool $ left `op` right

unpackNum :: LispVal -> ThrowsError Int
unpackNum (Number n) = return n
unpackNum (String s) = case readNum s of
                            Just n -> return n
                            Nothing -> throwError $ TypeMismatch "number" $ String s
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
 
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Boolean
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

symbolChecker :: Array LispVal -> ThrowsError LispVal
symbolChecker [Atom _] = return $ Bool true
symbolChecker _ = return $ Bool false

stringChecker :: Array LispVal -> ThrowsError LispVal
stringChecker [String _] = return $ Bool true
stringChecker _ = return $ Bool false

numberChecker :: Array LispVal -> ThrowsError LispVal
numberChecker [Number _] = return $ Bool true
numberChecker _ = return $ Bool false

boolChecker :: Array LispVal -> ThrowsError LispVal
boolChecker [Bool _] = return $ Bool true
boolChecker _ = return $ Bool false

sym2str :: Array LispVal -> ThrowsError LispVal
sym2str [Atom n] = return $ String $ show n
sym2str [v] = throwError $ TypeMismatch "symbol" v
sym2str vs = throwError $ NumArgs 1 vs

str2sym :: Array LispVal -> ThrowsError LispVal
str2sym [String s] = return $ Atom s
str2sym [v] = throwError $ TypeMismatch "string" v
str2sym vs = throwError $ NumArgs 1 vs

-- List primitives
car :: Array LispVal -> ThrowsError LispVal
car [List xs] = case head xs of
                     Just x -> return x
                     Nothing -> throwError $ NumArgs 1 []
car [DottedList xs _] = case head xs of
                             Just x -> return x
                             Nothing -> throwError $ NumArgs 1 []
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: Array LispVal -> ThrowsError LispVal
cdr [List xs] = case tail xs of
                     Just xss -> return $ List xss
                     Nothing -> throwError $ NumArgs 1 []
cdr [DottedList xs x] = case tail xs of
                             Just [singleton] -> return x
                             Just xss -> return $ DottedList xss x
                             Nothing -> throwError $ NumArgs 1 []
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: Array LispVal -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs xf] = return $ DottedList (x:xs) xf
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList
 
eqv :: Array LispVal -> ThrowsError LispVal
eqv [Bool b, Bool b'] = return $ Bool (b == b')
eqv [Number m, Number n] = return $ Bool (m == n)
eqv [String s, String s'] = return $ Bool (s == s')
eqv [Atom p, Atom q] = return $ Bool (p == q)
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List xs, List ys] = return $ Bool $ (length xs == length ys) && (all eqPair $ zip xs ys) where
  eqPair :: Tuple LispVal LispVal -> Boolean
  eqPair (Tuple v v') = case eqv [v, v'] of
                              Left err -> false
                              Right (Bool val) -> val
eqv [_, _] = return $ Bool false
eqv badArgList = throwError $ NumArgs 2 badArgList

--rep :: String -> String
--rep = show <<< eval <<< readExpr
--rep = readExpr >=> eval >=> show
-- readExpr :: String -> ThrowsError LispVal
-- eval :: LispVal -> ThrowsError LispVal
-- show :: (Show a) => a -> String
-- (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c 
