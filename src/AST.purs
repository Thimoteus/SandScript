module SandScript.AST where

import Prelude

import Control.Monad.Error.Class (catchError)

import Data.Either (Either(..))
import Data.List (List)
import Data.Foldable (class Foldable, intercalate)
import Data.StrMap (StrMap)

import Partial.Unsafe (unsafePartial)

import Text.Parsing.Parser (ParseError(..))

data WFF = Atom String
         | Integer Int
         | Float Number
         | String String
         | Bool Boolean
         | List (List WFF)
         | Vector (Array WFF)
         | PrimitiveFunc (PrimFn WFF)
         | Func { params :: List String
                , body :: WFF
                , closure :: StrMap WFF
              }

instance showWFF :: Show WFF where
  show (Atom n) = n
  show (Integer n) = show n
  show (Float n) = show n
  show (String s) = show s
  show (Bool true) = "True"
  show (Bool _) = "False"
  show (List xs) = "(" <> unwordsList xs <> ")"
  show (Vector xs) = "[" <> unwordsList xs <> "]"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func _) = "function"

unwordsList :: forall a f. (Show a, Foldable f, Functor f) => f a -> String
unwordsList = intercalate " " <<< map show

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
  show (ParseErr (ParseError e)) = "Parse error: " <> e.message
  show (BadSpecialForm msg form) = msg <> ": " <> show form
  show (NotFunction msg f) = msg <> ": " <> f
  show (UnboundVar msg v) = msg <> ": " <> v
  show (SetImmutable v) = "Trying to set multiple values for " <> v
  show (Default s) = s

type ThrowsError a = Either LangError a

type PrimFn a = List WFF -> ThrowsError a

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action ((pure <<< show) :: LangError -> ThrowsError String)

extractValue' :: forall a. Partial => ThrowsError a -> a
extractValue' (Right v) = v

extractValue :: forall a. ThrowsError a -> a
extractValue = unsafePartial extractValue'
