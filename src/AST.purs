module SandScript.AST
  ( WFF(..)
  , unwordsList
  ) where

import Prelude

import Data.List (List)
import Data.Foldable (intercalate)

data WFF = Atom String
         | Integer Int
         | String String
         | Bool Boolean
         | List (List WFF)
         | DotList (List WFF) WFF

instance showWFF :: Show WFF where
  show (Atom n) = n
  show (String s) = show s
  show (Integer n) = show n
  show (Bool true) = "True"
  show (Bool _) = "False"
  show (List xs) = "(" <> unwordsList xs <> ")"
  show (DotList xs x) = "(" <> unwordsList xs <> " . " <> show x <> ")"

unwordsList :: forall a. Show a => List a -> String
unwordsList = intercalate " " <<< map show
