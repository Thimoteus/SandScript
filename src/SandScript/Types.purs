module SandScript.Types where

import Prelude

import Data.Either
import Data.Maybe
import Data.Tuple

import Control.Monad.Error
import Control.Monad.Error.Trans
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Text.Parsing.Parser
import SandScript.Util

data LispVal = Atom String
             | List (Array LispVal)
             | DottedList (Array LispVal) LispVal
             | Number Int
             | String String
             | Bool Boolean

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
    | b = "#t"
    | otherwise = "#f"

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

type ThrowsError a = Either LispError a

type Env = Ref (Array (Tuple String (Ref LispVal)))

type REff r = Eff (ref :: REF | r)
type EffThrowsError r a = ErrorT LispError (REff r) a

fromNumber :: LispVal -> Maybe Int
fromNumber (Number n) = return n
fromNumber _ = Nothing

fromString :: LispVal -> Maybe String
fromString (String s) = return s
fromString _ = Nothing

fromBool :: LispVal -> Maybe Boolean
fromBool (Bool b) = return b
fromBool _ = Nothing

fromAtom :: LispVal -> Maybe String
fromAtom (Atom s) = return s
fromAtom _ = Nothing

