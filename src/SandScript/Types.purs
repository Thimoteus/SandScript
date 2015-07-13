module SandScript.Types where

import Prelude

import Data.Either
import Data.Maybe
import Data.Tuple

import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Console

import Text.Parsing.Parser
import SandScript.Util

type LispFH = ( ref :: REF, console :: CONSOLE, lispEff :: LispEff )

type ThrowsError a = Either LispError a

type Env = Ref (Array (Tuple String (Ref LispVal)))

type REff r = Eff (ref :: REF | r)

type EffThrowsError r a = ErrorT LispError (REff r) a
-- e = LispError, m = (REff r), a = a
type LispF = Eff LispFH

foreign import data LispEff :: !

data LispVal = Atom String
             | List (Array LispVal)
             | DottedList (Array LispVal) LispVal
             | Number Int
             | String String
             | Bool Boolean
             | PrimitiveFunc (Array LispVal -> ThrowsError LispVal)
             | Func { params :: Array String, varargs :: Maybe String
                    , body :: Array LispVal, closure :: Env }
             | EffFunc (Array LispVal -> EffThrowsError LispFH LispVal)

data LispError = NumArgs Int (Array LispVal)
               | TypeMismatch String LispVal
               | Parserr ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundedVar String String
               | PatternFail
               | ConditionalFail
               | Default String

instance showLispVal :: Show LispVal where
  show (Atom s) = s
  show (List [Atom "quote", x]) = "'" ++ show x
  show (List xs) = "(" ++ unwordsList xs ++ ")"
  show (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
  show (Number n) = show n
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
  eq (Number n) (Number m) = n == m
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
  show (PatternFail) = "Pattern match fail. Suggestion: include an `else` clause."
  show (ConditionalFail) = "Conditional fail. Suggestion: include an `else` clause."

instance lispError :: Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

fromNumber :: LispVal -> Maybe Int
fromNumber (Number n) = return n
fromNumber _ = Nothing
isNumber :: LispVal -> Boolean
isNumber (Number _) = true
isNumber _ = false

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

