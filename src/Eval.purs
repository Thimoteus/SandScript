module SandScript.Eval where

import Prelude

import SandScript.AST (WFF(..))
import SandScript.Errors (LangError(..), ThrowsError)

import Data.StrMap as Map
import Data.List as List
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Data.Maybe (maybe)

import Control.Monad.Error.Class (throwError)

infixr 5 List.Cons as :

eval :: WFF -> ThrowsError WFF
eval v@(String _) = pure v
eval n@(Integer _) = pure n
eval b@(Bool _) = pure b
eval (List (Atom "quote" : q : List.Nil)) = pure q
eval (List (Atom f : args)) = traverse eval args >>= apply f--apply f $ eval <$> args
eval bsf = throwError $ BadSpecialForm "Unrecognized special form" bsf

apply :: String -> List.List WFF -> ThrowsError WFF
apply f args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" f)
  (_ $ args)
  (Map.lookup f primitives)

data Op = Add | Sub | Mul | Div | Mod

primitives :: Map.StrMap (List.List WFF -> ThrowsError WFF)
primitives = Map.empty
           # Map.insert "+" (numericBinop Add)
           # Map.insert "-" (numericBinop Sub)
           # Map.insert "*" (numericBinop Mul)
           # Map.insert "/" (numericBinop Div)
           # Map.insert "mod" (numericBinop Mod)

numericBinop :: Op -> List.List WFF -> ThrowsError WFF
numericBinop _ List.Nil = throwError $ NumArgs 2 List.Nil
numericBinop _ x@(_ : List.Nil) = throwError $ NumArgs 2 x
numericBinop Add xs = Integer <<< foldl add 0 <$> traverse unpackInt xs
numericBinop Mul xs = Integer <<< foldl mul 1 <$> traverse unpackInt xs
numericBinop Sub xs = Integer <$> numericFold sub 0 xs
numericBinop Div xs = Integer <$> numericFold div 1 xs
numericBinop Mod xs = Integer <$> numericFold mod 0 xs

numericFold :: (Int -> Int -> Int) -> Int -> List.List WFF -> ThrowsError Int
numericFold op _ (x : xs) = do
  n <- unpackInt x
  ns <- traverse unpackInt xs
  pure $ foldl op n ns
numericFold op i _ = pure i

unpackInt :: WFF -> ThrowsError Int
unpackInt (Integer n) = pure n
unpackInt x = throwError $ TypeMismatch "integer" x
