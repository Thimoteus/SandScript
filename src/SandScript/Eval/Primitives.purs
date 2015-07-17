module SandScript.Eval.Primitives where

import Prelude

import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Array hiding (cons)
import qualified Data.Array.Unsafe as U
import qualified Data.String as S

import Control.Monad.Error.Class

import SandScript.Types
import SandScript.Util

primitives :: Array (Tuple String (Array LispVal -> ThrowsError LispVal))
primitives = [ "+" & overloadedPlus
             , "-" & overloadedMinus
             , "*" & overloadedTimes
             , "/" & overloadedDiv
             , "mod" & overloadedMod
             , "int?" & intChecker
             , "frac?" & fracChecker
             , "float?" & floatChecker
             , "complex?" & complexChecker
             , "number?" & numberChecker
             , "symbol?" & symbolChecker
             , "string?" & stringChecker
             , "bool?" & boolChecker
             -- N -> 2
             , "=" & numBoolBinop (==)
             , "<" & numBoolBinop (<)
             , "<" & numBoolBinop (<)
             , ">" & numBoolBinop (>)
             , "!=" & numBoolBinop (/=)
             , ">=" & numBoolBinop (>=)
             , "<=" & numBoolBinop (<=)
             -- 2 -> 2
             , "&&" & boolBoolBinop (&&)
             , "||" & boolBoolBinop (||)
             --, Tuple "not" boolNot
             -- String -> 2
             , "string=?" & strBoolBinop (==)
             , "string<?" & strBoolBinop (<)
             , "string>?" & strBoolBinop (>)
             , "string=?" & strBoolBinop (>=)
             , "string<=?" & strBoolBinop (<=)
             , "string>=?" & strBoolBinop (>=)
             -- String -> Something
             , "make-string" & makeStr
             , "string-length" & strLength
             , "string-ref" & strRef
             , "substring" & substr
             , "string-append" & strAppend
             , "string->list" & str2list
             , "list->string" & list2str
             -- list operators
             , "head" & car
             , "tail" & cdr
             , "cons" & cons
             , "ind" & ind
             -- equality checking
             , "eq?" & eqv
             , "eqv?" & eqv
             -- String -> String
             , "symbol->string" & sym2str
             , "string->symbol" & str2sym ]

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

overloadedPlus :: Array LispVal -> ThrowsError LispVal
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

overloadedMinus :: Array LispVal -> ThrowsError LispVal
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

overloadedTimes :: Array LispVal -> ThrowsError LispVal
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

overloadedDiv :: Array LispVal -> ThrowsError LispVal
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

overloadedMod :: Array LispVal -> ThrowsError LispVal
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

boolBinop :: forall a. (LispVal -> ThrowsError a) -> (a -> a -> Boolean) -> Array LispVal -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                               left <- unpacker $ args `U.unsafeIndex` 0
                               right <- unpacker $ args `U.unsafeIndex` 1
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

symbolChecker :: Array LispVal -> ThrowsError LispVal
symbolChecker [Atom _] = return $ Bool true
symbolChecker _ = return $ Bool false

stringChecker :: Array LispVal -> ThrowsError LispVal
stringChecker [String _] = return $ Bool true
stringChecker _ = return $ Bool false

intChecker :: Array LispVal -> ThrowsError LispVal
intChecker [Int _] = return $ Bool true
intChecker _ = return $ Bool false

floatChecker :: Array LispVal -> ThrowsError LispVal
floatChecker [Float _] = return $ Bool true
floatChecker _ = return $ Bool false

fracChecker :: Array LispVal -> ThrowsError LispVal
fracChecker [Frac _] = return $ Bool true
fracChecker _ = return $ Bool false

complexChecker :: Array LispVal -> ThrowsError LispVal
complexChecker [Complex _] = return $ Bool true
complexChecker _ = return $ Bool false

numberChecker :: Array LispVal -> ThrowsError LispVal
numberChecker val = return $ Bool $ any isTrue $ map ($ val) [intChecker, floatChecker, fracChecker, complexChecker]
  where
  isTrue :: ThrowsError LispVal -> Boolean
  isTrue (Right (Bool b)) = b
  isTrue _ = false

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

-- Bool stuff

--boolNot :: Array LispVal -> ThrowsError LispVal
--boolNot [Bool b] = return $ Bool (not b)
--boolNot [notBool] = throwError $ TypeMismatch "bool" notBool
--boolNot badArgs = throwError $ NumArgs 1 badArgs

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

ind :: Array LispVal -> ThrowsError LispVal
ind [Int i, List xs] = maybe (return $ List [])
                                (\ b -> return b)
                                (xs !! i)
ind [notInt, List _] = throwError $ TypeMismatch "int" notInt
ind [Int _, notList] = throwError $ TypeMismatch "list" notList
ind badArgs = throwError $ NumArgs 2 badArgs

-- equality

eqv :: Array LispVal -> ThrowsError LispVal
eqv [Bool b, Bool b'] = return $ Bool (b == b')
eqv [Int m, Int n] = return $ Bool (m == n)
eqv [p@(Frac _), q@(Frac _)] = return $ Bool (simplifyFrac p == simplifyFrac q)
eqv [Float m, Float n] = return $ Bool (m == n)
eqv [Complex z1, Complex z2] = return $ Bool (z1.real == z2.real && z1.imaginary == z2.imaginary)
eqv [String s, String s'] = return $ Bool (s == s')
eqv [Atom p, Atom q] = return $ Bool (p == q)
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List xs, List ys] = return $ Bool $ (length xs == length ys) && (all eqPair $ zip xs ys) where
  eqPair :: Tuple LispVal LispVal -> Boolean
  eqPair (Tuple v v') = case eqv [v, v'] of
                             Left _ -> false
                             Right (Bool val) -> val
eqv [_, _] = return $ Bool false
eqv badArgList = throwError $ NumArgs 2 badArgList

-- string stuff
makeStr :: Array LispVal -> ThrowsError LispVal
makeStr [Int n, String c] = case S.toChar c of
                                    Just chr -> return $ String <<< S.fromCharArray $ replicate n chr
                                    Nothing -> throwError $ BadSpecialForm "Expected singleton string, found" (String c)
makeStr [notInt, String _] = throwError $ TypeMismatch "int" notInt
makeStr [Int _, notString] = throwError $ TypeMismatch "string" notString
makeStr [x, y] = throwError $ BadSpecialForm "Incorrect `make-string` syntax" $ List [x, y]
makeStr badArgs = throwError $ NumArgs 2 badArgs

strLength :: Array LispVal -> ThrowsError LispVal
strLength [String s] = return $ Int $ S.length s
strLength [notString] = throwError $ TypeMismatch "string" notString
strLength badArgs = throwError $ NumArgs 1 badArgs

strRef :: Array LispVal -> ThrowsError LispVal
strRef [String s, n@(Int k)] = case S.charAt k s of
                                   Just c -> return $ String $ S.singleton c
                                   Nothing -> throwError $ TypeMismatch ("index smaller than " ++ (show $ S.length s)) n
strRef [notString, Int _] = throwError $ TypeMismatch "string" notString
strRef [String _, notInt] = throwError $ TypeMismatch "int" notInt
strRef [x, y] = throwError $ BadSpecialForm "Incorrect `string-ref` syntax" $ List [x, y]
strRef badArgs = throwError $ NumArgs 2 badArgs

substr :: Array LispVal -> ThrowsError LispVal
substr [String s, i@(Int start), f@(Int end)]
  | 0 <= start && start <= end && end <= S.length s = return $ String (s # S.take end # S.drop start)
  | otherwise = throwError $ TypeMismatch ("indices between 0 and " ++ (show $ S.length s)) (List [i, f])
substr [notString, Int _, Int _] = throwError $ TypeMismatch "string" notString
substr [_, notInt, Int _] = throwError $ TypeMismatch "int" notInt
substr [_, _, notInt] = throwError $ TypeMismatch "int" notInt
substr [x, y, z] = throwError $ BadSpecialForm "Incorrect `substring` syntax" $ List [x,y,z]
substr badArgs = throwError $ NumArgs 3 badArgs

strAppend :: Array LispVal -> ThrowsError LispVal
strAppend [] = throwError $ NumArgs 1 []
strAppend args
  | isJust $ traverse fromString args = return $ String $ foldl (\acc (String s) -> acc ++ s) "" args
  | otherwise = throwError $ TypeMismatch "list of strings" $ List args

str2list :: Array LispVal -> ThrowsError LispVal
str2list [String s] = return $ List <<< map String $ toChars s
str2list [notString] = throwError $ TypeMismatch "string" notString
str2list badArgs = throwError $ NumArgs 1 badArgs

list2str :: Array LispVal -> ThrowsError LispVal
list2str [List ss]  = strAppend ss
list2str [notList] = throwError $ TypeMismatch "list" notList
list2str badArgs = throwError $ NumArgs 1 badArgs
