module SandScript.Errors where

import Prelude

import SandScript.AST (WFF, unwordsList)

import Text.Parsing.Parser (ParseError)

import Data.Either (Either(..))
import Data.List (List)

import Partial.Unsafe (unsafePartial)

import Control.Monad.Error.Class (catchError)

data LangError = NumArgs Int (List WFF)
               | TypeMismatch String WFF
               | ParseErr ParseError
               | BadSpecialForm String WFF
               | NotFunction String String
               | UnboundVar String String
               | SetImmutable String
               | Default String

instance showLangError :: Show LangError where
  show (NumArgs n xs) = "Expected " <> show n <> " args, found values " <> unwordsList xs
  show (TypeMismatch s wff) = "Invalid type: expected " <> s <> " but found " <> show wff
  show (ParseErr perr) = "Parse error at " <> show perr
  show (BadSpecialForm msg form) = msg <> ": " <> show form
  show (NotFunction msg f) = msg <> ": " <> show f
  show (UnboundVar msg v) = msg <> ": " <> show v
  show (SetImmutable v) = "Trying to set multiple values for " <> v
  show (Default s) = s

type ThrowsError a = Either LangError a

type Eval a = List WFF -> ThrowsError a

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action ((pure <<< show) :: LangError -> ThrowsError String)

extractValue' :: forall a. Partial => ThrowsError a -> a
extractValue' (Right v) = v

extractValue :: forall a. ThrowsError a -> a
extractValue = unsafePartial extractValue'
