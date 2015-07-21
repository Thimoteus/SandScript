module SandScript.Eval.Primitives where

import Prelude

import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.List
import qualified Data.Array as A
import qualified Data.List.Unsafe as U
import qualified Data.String as S
import qualified Data.Char as C

import Control.Monad.Error.Class

import SandScript.Types
import SandScript.Util

primitives :: List (Tuple String (List LispVal -> ThrowsError LispVal))
primitives = "+" & overloadedPlus
           : "-" & overloadedMinus
           : "*" & overloadedTimes
           : "/" & overloadedDiv
           : "mod" & overloadedMod
           : "int?" & intChecker
           : "frac?" & fracChecker
           : "float?" & floatChecker
           : "complex?" & complexChecker
           : "number?" & numberChecker
           : "symbol?" & symbolChecker
           : "string?" & stringChecker
           : "bool?" & boolChecker
           -- N -> 2
           : "=" & numBoolBinop (==)
           : "<" & numBoolBinop (<)
           : "<" & numBoolBinop (<)
           : ">" & numBoolBinop (>)
           : "!=" & numBoolBinop (/=)
           : ">=" & numBoolBinop (>=)
           : "<=" & numBoolBinop (<=)
           -- 2 -> 2
           : "&&" & boolBoolBinop (&&)
           : "||" & boolBoolBinop (||)
           --, Tuple "not" boolNot
           -- String -> 2
           : "string=?" & strBoolBinop (==)
           : "string<?" & strBoolBinop (<)
           : "string>?" & strBoolBinop (>)
           : "string=?" & strBoolBinop (>=)
           : "string<=?" & strBoolBinop (<=)
           : "string>=?" & strBoolBinop (>=)
           -- String -> Something
           : "make-string" & makeStr
           : "string-length" & strLength
           : "string-ref" & strRef
           : "substring" & substr
           : "string-append" & strAppend
           : "string->list" & str2list
           : "list->string" & list2str
           -- list operators
           : "list?" & isList
           : "head" & car
           : "tail" & cdr
           : "ind" & ind
           -- equality checking
           : "eq?" & eqv
           : "eqv?" & eqv
           -- String -> String
           : "symbol->string" & sym2str
           : "string->symbol" & str2sym
           -- vectors
           : "vector?" & isVector
           : "vector->list" & vec2lst
           : "list->vector" & lst2vec
           : "v-range" & vrange
           -- both vectors and lists
           : "cons" & cons
           : "sort" & gsort
           : Nil

fracArith :: Op -> Tuple Int Int -> Tuple Int Int -> Tuple Int Int
fracArith Add (Tuple a b) (Tuple c d) = (a*d + b*c) & (b * d)
fracArith Sub (Tuple a b) (Tuple c d) = fracArith Add (a & b) (negate c & d)
fracArith Mul (Tuple a b) (Tuple c d) = (a*c) & (b*d)
fracArith Div (Tuple a b) (Tuple c d) = fracArith Mul (a & b) (d & c)

imagArith :: Op -> Tuple Number Number -> Tuple Number Number -> Tuple Number Number
imagArith Add (Tuple x1 y1) (Tuple x2 y2) = (x1 + x2) & (y1 + y2)
imagArith Sub (Tuple x1 y1) (Tuple x2 y2) = (x1 - x2) & (y1 - y2)
imagArith Mul (Tuple x1 y1) (Tuple x2 y2) = (x1*x2 - y1*y2) & (y1*x2 + x1*y2)
imagArith Div (Tuple x1 y1) (Tuple x2 y2) = ((x1*x2 + y1*y2)/(x2*x2 + y2*y2)) & ((y1*x2 - x1*y2)/(x2*x2 + y2*y2))

overloadedPlus :: List LispVal -> ThrowsError LispVal
overloadedPlus args
  | all isInt args = do
    xs <- traverse unpackInt args
    return $ Int $ foldl1 (+) xs
  | all isFloat args = do
    xs <- traverse unpackFloat args
    return $ Float $ foldl1 (+) xs
  | all isFrac args = do
    xs <- traverse unpackFrac args
    return $ simplifyFrac $ Frac $ foldl1 (fracArith Add) xs
  | all isComplex args = do
    xs <- traverse unpackComplex args
    let sum = foldl1 (imagArith Add) xs
    return $ Complex { real: fst sum, imaginary: snd sum }
  | otherwise = throwError $ TypeMismatch "same type of number" (List args)

overloadedMinus :: List LispVal -> ThrowsError LispVal
overloadedMinus args
  | all isInt args = do
    xs <- traverse unpackInt args
    return $ Int $ foldl1 (-) xs
  | all isFloat args = do
    xs <- traverse unpackFloat args
    return $ Float $ foldl1 (-) xs
  | all isFrac args = do
    xs <- traverse unpackFrac args
    return $ simplifyFrac $ Frac $ foldl1 (fracArith Sub) xs
  | all isComplex args = do
    xs <- traverse unpackComplex args
    let sum = foldl1 (imagArith Sub) xs
    return $ Complex { real: fst sum, imaginary: snd sum }
  | otherwise = throwError $ TypeMismatch "same type of number" (List args)

overloadedTimes :: List LispVal -> ThrowsError LispVal
overloadedTimes args
  | all isInt args = do
    xs <- traverse unpackInt args
    return $ Int $ foldl1 (*) xs
  | all isFloat args = do
    xs <- traverse unpackFloat args
    return $ Float $ foldl1 (*) xs
  | all isFrac args = do
    xs <- traverse unpackFrac args
    return $ simplifyFrac $ Frac $ foldl1 (fracArith Mul) xs
  | all isComplex args = do
    xs <- traverse unpackComplex args
    let prod = foldl1 (imagArith Mul) xs
    return $ Complex { real: fst prod, imaginary: snd prod }
  | otherwise = throwError $ TypeMismatch "same type of number" (List args)

overloadedDiv :: List LispVal -> ThrowsError LispVal
overloadedDiv args
  | all isInt args = do
    xs <- traverse unpackInt args
    return $ Int $ foldl1 div xs
  | all isFloat args = do
    xs <- traverse unpackFloat args
    return $ Float $ foldl1 (/) xs
  | all isFrac args = do
    xs <- traverse unpackFrac args
    return $ simplifyFrac $ Frac $ foldl1 (fracArith Div) xs
  | all isComplex args = do
    xs <- traverse unpackComplex args
    let divd = foldl1 (imagArith Div) xs
    return $ Complex { real: fst divd, imaginary: snd divd }
  | otherwise = throwError $ TypeMismatch "same type of number" (List args)

overloadedMod :: List LispVal -> ThrowsError LispVal
overloadedMod args
  | all isInt args = do
    xs <- traverse unpackInt args
    return $ Int $ foldl1 mod xs
  | otherwise = throwError $ TypeMismatch "int" (List args)

unpackInt :: LispVal -> ThrowsError Int
unpackInt (Int n) = return n
unpackInt notNum = throwError $ TypeMismatch "number" notNum

unpackFloat :: LispVal -> ThrowsError Number
unpackFloat (Float n) = return n
unpackFloat notFloat = throwError $ TypeMismatch "float" notFloat

unpackFrac :: LispVal -> ThrowsError (Tuple Int Int)
unpackFrac (Frac p@(Tuple _ _)) = return p
unpackFrac notFrac = throwError $ TypeMismatch "frac" notFrac

unpackComplex :: LispVal -> ThrowsError (Tuple Number Number)
unpackComplex (Complex z) = return $ z.real & z.imaginary
unpackComplex notComplex = throwError $ TypeMismatch "complex" notComplex

boolBinop :: forall a. (LispVal -> ThrowsError a) -> (a -> a -> Boolean) -> List LispVal -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                               left <- unpacker $ U.head args
                               right <- unpacker $ U.head $ U.tail args
                               return $ Bool $ left `op` right

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Boolean
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

numBoolBinop = boolBinop unpackInt
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

symbolChecker :: List LispVal -> ThrowsError LispVal
symbolChecker (Cons (Atom _) Nil) = return $ Bool true
symbolChecker _ = return $ Bool false

stringChecker :: List LispVal -> ThrowsError LispVal
stringChecker (Cons (String _) Nil) = return $ Bool true
stringChecker _ = return $ Bool false

intChecker :: List LispVal -> ThrowsError LispVal
intChecker (Cons (Int _) Nil) = return $ Bool true
intChecker _ = return $ Bool false

floatChecker :: List LispVal -> ThrowsError LispVal
floatChecker (Cons (Float _) Nil) = return $ Bool true
floatChecker _ = return $ Bool false

fracChecker :: List LispVal -> ThrowsError LispVal
fracChecker (Cons (Frac _) Nil) = return $ Bool true
fracChecker _ = return $ Bool false

complexChecker :: List LispVal -> ThrowsError LispVal
complexChecker (Cons (Complex _) Nil) = return $ Bool true
complexChecker _ = return $ Bool false

numberChecker :: List LispVal -> ThrowsError LispVal
numberChecker val = return $ Bool $ any isTrue $ map ($ val) [intChecker, floatChecker, fracChecker, complexChecker]
  where
  isTrue :: ThrowsError LispVal -> Boolean
  isTrue (Right (Bool b)) = b
  isTrue _ = false

boolChecker :: List LispVal -> ThrowsError LispVal
boolChecker (Cons (Bool _) Nil) = return $ Bool true
boolChecker _ = return $ Bool false

sym2str :: List LispVal -> ThrowsError LispVal
sym2str (Cons (Atom n) Nil) = return $ String $ show n
sym2str (Cons v Nil) = throwError $ TypeMismatch "symbol" v
sym2str vs = throwError $ NumArgs 1 vs

str2sym :: List LispVal -> ThrowsError LispVal
str2sym (Cons (String s) Nil) = return $ Atom s
str2sym (Cons v Nil) = throwError $ TypeMismatch "string" v
str2sym vs = throwError $ NumArgs 1 vs

-- Bool stuff

--boolNot :: List LispVal -> ThrowsError LispVal
--boolNot [Bool b] = return $ Bool (not b)
--boolNot [notBool] = throwError $ TypeMismatch "bool" notBool
--boolNot badArgs = throwError $ NumArgs 1 badArgs

-- List primitives

isList :: List LispVal -> ThrowsError LispVal
isList (Cons (List _) Nil) = return $ Bool true
isList _ = return $ Bool false

car :: List LispVal -> ThrowsError LispVal
car (Cons (List (Cons x xs)) Nil) = return x
car (Cons (DottedList (Cons x xs) _) Nil) = return x
car (Cons (Vector xs) Nil) = case A.head xs of
                                  Just x -> return x
                                  Nothing -> throwError $ NumArgs 1 Nil
car (Cons badArg Nil) = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: List LispVal -> ThrowsError LispVal
cdr (Cons (List (Cons _ xs)) Nil) = return $ List xs
cdr (Cons (DottedList (Cons _ Nil) x) Nil) = return x
cdr (Cons (DottedList (Cons _ xs) x) Nil) = return $ DottedList xs x
cdr (Cons (Vector xs) Nil) = case A.tail xs of
                                  Just xss -> return $ Vector xss
                                  _ -> throwError $ NumArgs 1 Nil
cdr (Cons (badArg) Nil) = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

ind :: List LispVal -> ThrowsError LispVal
ind (Cons (Int i) (Cons (List xs) Nil)) = maybe (return $ List Nil)
                                                (\ b -> return b)
                                                (xs !! i)
ind (Cons (Int i) (Cons (Vector xs) Nil)) = maybe (return $ Vector [])
                                                  (\ b -> return b)
                                                  (xs A.!! i)
ind (Cons notInt (Cons (List _) Nil)) = throwError $ TypeMismatch "int" notInt
ind (Cons (Int _) (Cons notList Nil)) = throwError $ TypeMismatch "list or vector" notList
ind badArgs = throwError $ NumArgs 2 badArgs

-- equality

eqv :: List LispVal -> ThrowsError LispVal
eqv (Cons (Bool b) (Cons (Bool b') Nil)) = return $ Bool (b == b')
eqv (Cons (Int m) (Cons (Int n) Nil)) = return $ Bool (m == n)
eqv (Cons p@(Frac _) (Cons q@(Frac _) Nil)) = return $ Bool (simplifyFrac p == simplifyFrac q)
eqv (Cons (Float m) (Cons (Float n) Nil)) = return $ Bool (m == n)
eqv (Cons (Complex z1) (Cons (Complex z2) Nil)) = return $ Bool (z1.real == z2.real && z1.imaginary == z2.imaginary)
eqv (Cons (String s) (Cons (String s') Nil)) = return $ Bool (s == s')
eqv (Cons (Atom p) (Cons (Atom q) Nil)) = return $ Bool (p == q)
eqv (Cons (DottedList xs x) (Cons (DottedList ys y) Nil)) = eqv ((List $ snoc xs x) : (List $ snoc ys y) : Nil)
eqv (Cons (List xs) (Cons (List ys) Nil)) = return $ Bool $ (length xs == length ys) && (all eqPair $ zip xs ys)
eqv (Cons (Vector xs) (Cons (Vector ys) Nil)) = return $ Bool $ (A.length xs == A.length ys) && (all eqPair $ A.zip xs ys)
eqv (Cons _ (Cons _ Nil)) = return $ Bool false
eqv badArgList = throwError $ NumArgs 2 badArgList

eqPair :: Tuple LispVal LispVal -> Boolean
eqPair (Tuple v v') = case eqv (v : v' : Nil) of
                           Left _ -> false
                           Right (Bool v) -> v

-- string stuff
makeStr :: List LispVal -> ThrowsError LispVal
makeStr (Cons (Int n) (Cons (String c) Nil)) = case S.toChar c of
                                                    Just chr -> return $ String $ foldMap C.toString (replicate n chr)
                                                    Nothing -> throwError $ BadSpecialForm "Expected singleton string, found" (String c)
makeStr (Cons (notInt) (Cons (String _) Nil)) = throwError $ TypeMismatch "int" notInt
makeStr (Cons (Int _) (Cons notString Nil)) = throwError $ TypeMismatch "string" notString
makeStr (Cons x (Cons y Nil)) = throwError $ BadSpecialForm "Incorrect `make-string` syntax. First argument should be an int, second should be a singleton string" $ List (x:y:Nil)
makeStr badArgs = throwError $ NumArgs 2 badArgs

strLength :: List LispVal -> ThrowsError LispVal
strLength (Cons (String s) Nil) = return $ Int $ S.length s
strLength (Cons notString Nil) = throwError $ TypeMismatch "string" notString
strLength badArgs = throwError $ NumArgs 1 badArgs

strRef :: List LispVal -> ThrowsError LispVal
strRef (Cons (String s) (Cons n@(Int k) Nil)) = case S.charAt k s of
                                                     Just c -> return $ String $ S.singleton c
                                                     Nothing -> throwError $ TypeMismatch ("index smaller than " ++ (show $ S.length s)) n
strRef (Cons notString (Cons (Int _) Nil)) = throwError $ TypeMismatch "string" notString
strRef (Cons (String _) (Cons notInt Nil)) = throwError $ TypeMismatch "int" notInt
strRef (Cons x (Cons y Nil)) = throwError $ BadSpecialForm "Incorrect `string-ref` syntax" $ List (x:y:Nil)
strRef badArgs = throwError $ NumArgs 2 badArgs

substr :: List LispVal -> ThrowsError LispVal
substr (Cons (String s) (Cons i@(Int start) (Cons f@(Int end) Nil)))
  | 0 <= start && start <= end && end <= S.length s = return $ String $ S.drop start $ S.take end s
  | otherwise = throwError $ TypeMismatch ("indices between 0 and " ++ (show $ S.length s)) (List (i:f:Nil))
substr (Cons x (Cons y (Cons z Nil))) = throwError $ BadSpecialForm "Incorrect `substring` syntax. First argument should be a string, second and third should be ints." $ List (x:y:z:Nil)
substr badArgs = throwError $ NumArgs 3 badArgs

strAppend :: List LispVal -> ThrowsError LispVal
strAppend Nil = throwError $ NumArgs 1 Nil
strAppend args
  | all isString args = return $ String $ foldl (\ acc (String s) -> acc ++ s) "" args
  | otherwise = throwError $ TypeMismatch "list of strings" $ List args

str2list :: List LispVal -> ThrowsError LispVal
str2list (Cons (String s) Nil) = return $ List $ map String $ toChars s
str2list (Cons notString Nil) = throwError $ TypeMismatch "string" notString
str2list badArgs = throwError $ NumArgs 1 badArgs

list2str :: List LispVal -> ThrowsError LispVal
list2str (Cons (List ss) Nil) = strAppend ss
list2str (Cons notList Nil) = throwError $ TypeMismatch "list" notList
list2str badArgs = throwError $ NumArgs 1 badArgs

-- vector stuff
isVector :: List LispVal -> ThrowsError LispVal
isVector (Cons (Vector _) Nil) = return $ Bool true
isVector _ = return $ Bool false

vec2lst :: List LispVal -> ThrowsError LispVal
vec2lst (Cons (Vector xs) Nil) = return $ List (toList xs)
vec2lst (Cons notVec Nil) = throwError $ TypeMismatch "vector" notVec
vec2lst badArgs = throwError $ NumArgs 1 badArgs

lst2vec :: List LispVal -> ThrowsError LispVal
lst2vec (Cons (List xs) Nil) = return $ Vector (fromList xs)
lst2vec (Cons notList Nil) = throwError $ TypeMismatch "list" notList
lst2vec badArgs = throwError $ NumArgs 1 badArgs

vrange :: List LispVal -> ThrowsError LispVal
vrange (Cons (Int start) (Cons (Int end) Nil)) = return $ Vector $ map Int $ A.range start end
vrange (Cons x (Cons y Nil)) = throwError $ BadSpecialForm "Incorrect `v-range` syntax. Arguments should be ints." $ Vector [x,y]
vrange badArgs = throwError $ NumArgs 2 badArgs

-- vectors and lists

cons :: List LispVal -> ThrowsError LispVal
cons (Cons x (Cons (List Nil) Nil)) = return $ List $ singleton x
cons (Cons x (Cons (List xs) Nil)) = return $ List (x:xs)
cons (Cons x (Cons (DottedList xs xf) Nil)) = return $ DottedList (x:xs) xf
cons (Cons x (Cons (Vector xs) Nil)) = return $ Vector $ A.cons x xs
cons (Cons x1 (Cons x2 Nil)) = return $ DottedList (singleton x1) x2
cons badArgList = throwError $ NumArgs 2 badArgList

conjImpl :: List LispVal -> ThrowsError LispVal
conjImpl (Cons x (Cons (List Nil) Nil)) = return $ List $ singleton x
conjImpl (Cons x (Cons (List xs) Nil)) = return $ List (x:xs)
conjImpl (Cons x (Cons (DottedList xs xf) Nil)) = return $ DottedList (x:xs) xf
conjImpl (Cons x (Cons (Vector xs) Nil)) = return $ Vector $ A.snoc xs x
conjImpl (Cons x1 (Cons x2 Nil)) = return $ DottedList (singleton x1) x2
conjImpl badArgList = throwError $ NumArgs 2 badArgList

gsort :: List LispVal -> ThrowsError LispVal
gsort (Cons (Vector xs) Nil) = return $ Vector $ A.sort xs
gsort (Cons (List xs) Nil) = return $ List $ sort xs
gsort (Cons whatisthis Nil) = throwError $ TypeMismatch "list or vector" whatisthis
gsort wrong = throwError $ NumArgs 2 wrong
