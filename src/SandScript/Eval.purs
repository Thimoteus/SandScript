module SandScript.Eval where

import Prelude

import Data.Maybe
import Data.Tuple
import qualified Data.String as S
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Array hiding (cons)
import Data.Array.Unsafe (unsafeIndex)

import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Bind ((=<<))

import SandScript.Types
import SandScript.Errors
import SandScript.Util
import SandScript.Parser
import SandScript.Variables

eval :: forall r. Env -> LispVal -> EffThrowsError r LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
-- variables
eval env (Atom id) = getVar env id
-- special list stuff
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = evalIf env pred conseq alt
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
-- general list stuff
eval env (List ls) = case uncons ls of
                          -- Just { head = Atom "case", tail = args } -> evalCase env args
                          -- Just { head = Atom "cond", tail = args } -> evalCond env args
                          Just { head = Atom func, tail = args } -> liftThrows <<< fapply func =<< traverse (eval env) args
                          _ -> throwError $ NotFunction "Did not recognize function" (show $ List ls)
eval env badform = throwError $ BadSpecialForm "Unrecognized special form" badform

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
             -- String -> Something
             , Tuple "make-string" makeStr
             , Tuple "string-length" strLength
             , Tuple "string-ref" strRef
             , Tuple "substring" substr
             , Tuple "string-append" strAppend
             , Tuple "string->list" str2list
             , Tuple "list->string" list2str
             -- list operators
             , Tuple "car" car
             , Tuple "head" car
             , Tuple "cdr" cdr
             , Tuple "tail" cdr
             , Tuple "cons" cons
             , Tuple "eq?" eqv
             , Tuple "eqv?" eqv
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
                             Left _ -> false
                             Right (Bool val) -> val
eqv [_, _] = return $ Bool false
eqv badArgList = throwError $ NumArgs 2 badArgList

-- conditional stuff
evalIf :: forall r. Env -> LispVal -> LispVal -> LispVal -> EffThrowsError r LispVal
evalIf env pred conseq alt = do
  result <- eval env pred
  case result of
       Bool false -> eval env alt
       Bool true -> eval env conseq
       notBool -> throwError $ TypeMismatch "bool" notBool

{--
evalCase :: forall r. Env -> Array LispVal -> EffThrowsError r LispVal
evalCase env as = case uncons as of
                       Just xs -> ecase env xs.head xs.tail
                       Nothing -> throwError $ NumArgs 1 []

ecase :: forall r. Env -> LispVal -> Array LispVal -> EffThrowsError r LispVal
ecase _ _ [] = throwError PatternFail
ecase env _ [List [Atom "else", val]] = eval env val
ecase env key clauses = case uncons clauses of
                             Just { head = List [List datums, val], tail = rest } -> if eqv' env key datums
                                                                                        then return val
                                                                                        else ecase env key rest
                             Just xs -> throwError $ BadSpecialForm "Incorrect `case` syntax" xs.head
                             Nothing -> throwError $ NumArgs 2 clauses

eqv' :: Env -> LispVal -> Array LispVal -> Boolean
eqv' env k ds = let pairs :: Array (Array LispVal)
                    pairs = zipWith (\ a b -> [a, b]) (replicate (length ds) k) ds
                    evaled :: forall r. Array (EffThrowsError r (Array LispVal))
                    evaled = map (traverse (eval env)) pairs
                    flipped :: forall r. EffThrowsError r (Array (Array LispVal))
                    flipped = sequence evaled
                    ran :: forall r. (REff r) (Either LispError (Array (Array LispVal) ))
                    ran = runErrorT flipped
                 in case flipped of
                         Left _ -> false
                         Right evald -> either (const false) (\ bs -> or $ map (\ (Bool b) -> b) bs) (traverse eqv evald)
evalCond :: forall r. Env -> Array LispVal -> EffThrowsError r LispVal
evalCond env clauses = case uncons clauses of
                            Just { head = List [Atom "else", expr],  tail = [] } -> eval env expr
                            Just { head = List [test@(List ts), expr], tail = rest } -> condChecker env test expr rest
                            Just { head = List [test@(List ts)], tail = rest } -> condChecker env test test rest
                            Just xs -> throwError $ BadSpecialForm "Incorrect `cond` syntax" xs.head
                            _ -> throwError ConditionalFail

condChecker :: forall r. Env -> LispVal -> LispVal -> Array LispVal -> EffThrowsError r LispVal
condChecker env test expr rest
  | either (const false) (\ (Bool b) -> b) (eval test) = eval expr -- TODO: (eval test) !:: Either _ _, but ErorrT _ _ _
  | otherwise = evalCond env rest
  --}
-- string stuff
makeStr :: Array LispVal -> ThrowsError LispVal
makeStr [Number n, String c] = case S.toChar c of
                                    Just chr -> return $ String <<< S.fromCharArray $ replicate n chr
                                    Nothing -> throwError $ BadSpecialForm "Expected singleton string, found" (String c)
makeStr [notNumber, String _] = throwError $ TypeMismatch "number" notNumber
makeStr [Number _, notString] = throwError $ TypeMismatch "string" notString
makeStr [x, y] = throwError $ BadSpecialForm "Incorrect `make-string` syntax" $ List [x, y]
makeStr badArgs = throwError $ NumArgs 2 badArgs

strLength :: Array LispVal -> ThrowsError LispVal
strLength [String s] = return $ Number $ S.length s
strLength [notString] = throwError $ TypeMismatch "string" notString
strLength badArgs = throwError $ NumArgs 1 badArgs

strRef :: Array LispVal -> ThrowsError LispVal
strRef [String s, n@(Number k)] = case S.charAt k s of
                                   Just c -> return $ String $ S.singleton c
                                   Nothing -> throwError $ TypeMismatch ("index smaller than " ++ (show $ S.length s)) n
strRef [notString, Number _] = throwError $ TypeMismatch "string" notString
strRef [String _, notNumber] = throwError $ TypeMismatch "number" notNumber
strRef [x, y] = throwError $ BadSpecialForm "Incorrect `string-ref` syntax" $ List [x, y]
strRef badArgs = throwError $ NumArgs 2 badArgs

substr :: Array LispVal -> ThrowsError LispVal
substr [String s, i@(Number start), f@(Number end)]
  | 0 <= start && start <= end && end <= S.length s = return $ String (s # S.take end # S.drop start)
  | otherwise = throwError $ TypeMismatch ("indices between 0 and " ++ (show $ S.length s)) (List [i, f])
substr [notString, Number _, Number _] = throwError $ TypeMismatch "string" notString
substr [_, notNumber, Number _] = throwError $ TypeMismatch "number" notNumber
substr [_, _, notNumber] = throwError $ TypeMismatch "number" notNumber
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
