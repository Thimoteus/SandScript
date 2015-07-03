module SandScript.Types where

import SandScript.Util
import Text.Parsing.Parser
import Control.Monad.Error
import Data.Either

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Number
             | String String
             | Bool Boolean

data LispError = NumArgs Number [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundedVar String String
               | Default String

instance showLispVal :: Show LispVal where
  show (Atom s) = s
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
  show (Parser parserr) = "Parse error at " ++ show parserr
  show (BadSpecialForm msg form) = msg ++ ": " ++ show form
  show (NotFunction msg func) = msg ++ ": " ++ func
  show (UnboundedVar msg varname) = msg ++ ": " ++ varname

instance lispError :: Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError
