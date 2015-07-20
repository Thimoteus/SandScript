module SandScript.Types where

import Prelude
import Math hiding (log)

import Data.Either
import Data.Maybe
import Data.Tuple
import Data.List
import qualified Data.Int as I

import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console
import qualified Control.Monad.Eff.Exception as Exc

import Node.FS
import Node.FS.Sync
import Text.Parsing.Parser
import SandScript.Util

type LispFH = ( ref :: REF, console :: CONSOLE, fs :: FS, err :: Exc.EXCEPTION )

type ThrowsError a = Either LispError a

type Env = Ref (List (Tuple String (Ref LispVal)))

type EffThrowsError a = ErrorT LispError LispF a

type LispF = Eff LispFH

data LispVal = Atom String
             | List (List LispVal)
             | DottedList (List LispVal) LispVal
             | Vector (Array LispVal)
             | Int Int
             | Float Number
             | Frac (Tuple Int Int)
             | Complex { real :: Number, imaginary :: Number }
             | String String
             | Bool Boolean
             | PrimitiveFunc (List LispVal -> ThrowsError LispVal)
             | Func { params :: List String, varargs :: Maybe String
                    , body :: List LispVal, closure :: Env }
             | EffFunc (List LispVal -> EffThrowsError LispVal)
             | Port FileDescriptor

data LispError = NumArgs Int (List LispVal)
               | TypeMismatch String LispVal
               | Parserr ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundedVar String String
               | PatternFail
               | ConditionalFail
               | Default String

data Op = Add | Sub | Mul | Div

instance showLispVal :: Show LispVal where
  show (Atom s) = s
  show (List (Cons (Atom "quote") (Cons x Nil))) = "'" ++ show x
  show (List xs) = "(" ++ unwordsList xs ++ ")"
  show (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
  show (Vector xs) = "[" ++ unwordsArray xs ++ "]"
  show (Int n)
    | n < 0  = "~" ++ show (absInt n)
    | otherwise = show n
  show (Float n)
    | n < 0.0 = "~" ++ show (abs n)
    | otherwise = show n
  show (Frac (Tuple p q))
    | p < 0 && q >= 0 = "~" ++ show (absInt p) ++ "/" ++ show q
    | p >= 0 && q < 0 = "~" ++ show p ++ "/" ++ show (absInt q)
    | p < 0 && q < 0 = show (absInt p) ++ "/" ++ show (absInt q)
    | otherwise = show p ++ "/" ++ show q
  show (Complex z)
    | z.real < 0.0 && z.imaginary < 0.0 = "~" ++ show (abs z.real) ++ "+~" ++ show (abs z.imaginary) ++ "i"
    | z.real >= 0.0 && z.imaginary < 0.0 = show z.real ++ "+~" ++ show (abs z.imaginary) ++ "i"
    | z.real < 0.0 && z.imaginary >= 0.0 = "~" ++ show (abs z.real) ++ "+" ++ show z.imaginary ++ "i"
    | otherwise = show z.real ++ "+" ++ show z.imaginary ++ "i"
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b)
    | b = "true"
    | otherwise = "false"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func { params = args, varargs = varargs, body = body, closure = env }) = "(lambda ("
       ++ (unwords $ map show args)
       ++ (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ... )"
  show (EffFunc _) = "<IO primitive>"

instance eqLispVal :: Eq LispVal where
  eq (Atom p) (Atom q) = p == q
  eq (List xs) (List ys) = xs == ys
  eq (DottedList xs x) (DottedList ys y) = xs == ys && x == y
  eq (Vector xs) (Vector ys) = xs == ys
  eq (Int n) (Int m) = n == m
  eq (Float n) (Float m) = n == m
  eq (Frac p) (Frac q) = p == q
  eq (String s) (String s') = s == s'
  eq (Bool b) (Bool b') = b == b'
  eq _ _ = false

instance showLispError :: Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args, found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parserr parserr) = "Parse error at " ++ show parserr
  show (BadSpecialForm msg form) = msg ++ ": " ++ show form
  show (NotFunction msg func) = msg ++ ": " ++ func
  show (UnboundedVar msg varname) = msg ++ ": " ++ varname
  show (PatternFail) = "Pattern match fail. Perhaps you didn't include an `else` clause?"
  show (ConditionalFail) = "Conditional fail. Perhaps you didn't include an `else` clause?"

instance lispError :: Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

fromInt :: LispVal -> Maybe Int
fromInt (Int n) = return n
fromInt _ = Nothing
isInt :: LispVal -> Boolean
isInt (Int _) = true
isInt _ = false

fromFloat :: LispVal -> Maybe Number
fromFloat (Float n) = return n
fromFloat _ = Nothing
isFloat :: LispVal -> Boolean
isFloat (Float _) = true
isFloat _ = false

fromFrac :: LispVal -> Maybe (Tuple Int Int)
fromFrac (Frac n) = return n
fromFrac _ = Nothing
isFrac :: LispVal -> Boolean
isFrac (Frac _) = true
isFrac _ = false

fromComplex :: LispVal -> Maybe { real :: Number, imaginary :: Number }
fromComplex (Complex z) = return z
fromComplex _ = Nothing
isComplex :: LispVal -> Boolean
isComplex (Complex _) = true
isComplex _ = false

fromString :: LispVal -> Maybe String
fromString (String s) = return s
fromString _ = Nothing
isString :: LispVal -> Boolean
isString (String _) = true
isString _ = false

fromBool :: LispVal -> Maybe Boolean
fromBool (Bool b) = return b
fromBool _ = Nothing
isBool :: LispVal -> Boolean
isBool (Bool _) = true
isBool _ = false

fromAtom :: LispVal -> Maybe String
fromAtom (Atom s) = return s
fromAtom _ = Nothing
isAtom :: LispVal -> Boolean
isAtom (Atom _) = true
isAtom _ = false

simplifyFrac :: LispVal -> LispVal
simplifyFrac (Frac (Tuple p q)) = Frac $ (p `div` gcd p q) & (q `div` gcd p q)
  where
  gcd :: Int -> Int -> Int
  gcd m n
    | n == 0 = m
    | otherwise = gcd n (m `mod` n)
