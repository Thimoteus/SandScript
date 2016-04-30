module SandScript.Primitives (primitives) where

import Prelude
import Data.List as List
import Data.StrMap as Map
import Control.Monad.Error.Class (throwError)
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import SandScript.AST (LangError(..), ThrowsError, WFF(..), PrimFn)

infixr 5 List.Cons as :

data Op = Add | Sub | Mul | Div | Mod

primitives :: Map.StrMap (PrimFn WFF)
primitives = Map.empty
           # Map.insert "+" (numericBinop Add)
           # Map.insert "-" (numericBinop Sub)
           # Map.insert "*" (numericBinop Mul)
           # Map.insert "/" (numericBinop Div)
           # Map.insert "%" (numericBinop Mod)
           # Map.insert "=" (intBoolBinop eq)
           # Map.insert "<" (intBoolBinop (<))
           # Map.insert ">" (intBoolBinop (>))
           # Map.insert "/=" (intBoolBinop (/=))
           # Map.insert ">=" (intBoolBinop (>=))
           # Map.insert "<=" (intBoolBinop (<=))
           # Map.insert "&&" (boolBoolBinop (&&))
           # Map.insert "||" (boolBoolBinop (||))
           # Map.insert "string=?" (strBoolBinop eq)
           # Map.insert "string<?" (strBoolBinop (<))
           # Map.insert "string>?" (strBoolBinop (>))
           # Map.insert "string<=?" (strBoolBinop (<=))
           # Map.insert "string>=?" (strBoolBinop (>=))
           # Map.insert "head" evalHead
           # Map.insert "tail" evalTail
           # Map.insert "cons" evalCons
           # Map.insert "==" eqv
           # Map.insert "eqv" eqv

numericBinop :: Op -> PrimFn WFF
numericBinop _ List.Nil = throwError $ NumArgs 2 List.Nil
numericBinop _ x@(_ : List.Nil) = throwError $ NumArgs 2 x
numericBinop Add xs = Integer <<< foldl add 0 <$> traverse unpackInt xs
numericBinop Mul xs = Integer <<< foldl mul 1 <$> traverse unpackInt xs
numericBinop Sub xs = Integer <$> numericFold sub 0 xs
numericBinop Div xs = Integer <$> numericFold div 1 xs
numericBinop Mod xs = Integer <$> numericFold mod 0 xs

numericFold :: (Int -> Int -> Int) -> Int -> PrimFn Int
numericFold op _ (x : xs) = do
  n <- unpackInt x
  ns <- traverse unpackInt xs
  pure $ foldl op n ns
numericFold op i _ = pure i

type Unpacker a = WFF -> ThrowsError a
type Operator a b = a -> a -> b
infixr 0 type Operator as ~~>

boolBinop :: forall a. Unpacker a -> a ~~> Boolean -> PrimFn WFF
boolBinop u op (x : y : _) = Bool <$> (op <$> u x <*> u y)
boolBinop _ _ as = throwError $ NumArgs 2 as

unpackStr :: Unpacker String
unpackStr (String s) = pure s
unpackStr x = throwError $ TypeMismatch "string" x

unpackBool :: Unpacker Boolean
unpackBool (Bool b) = pure b
unpackBool x = throwError $ TypeMismatch "bool" x

unpackInt :: Unpacker Int
unpackInt (Integer n) = pure n
unpackInt x = throwError $ TypeMismatch "integer" x

boolBoolBinop :: Boolean ~~> Boolean -> PrimFn WFF
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: String ~~> Boolean -> PrimFn WFF
strBoolBinop = boolBinop unpackStr

intBoolBinop :: Int ~~> Boolean -> PrimFn WFF
intBoolBinop = boolBinop unpackInt

evalHead :: PrimFn WFF
evalHead ((List (x : _)) : List.Nil) = pure x
evalHead (x : List.Nil) = throwError $ TypeMismatch "list" x
evalHead xs = throwError $ NumArgs 1 xs

evalTail :: PrimFn WFF
evalTail ((List (_ : xs)) : List.Nil) = pure $ List xs
evalTail (ba : List.Nil) = throwError $ TypeMismatch "pair" ba
evalTail xs = throwError $ NumArgs 1 xs

evalCons :: PrimFn WFF
evalCons (x : List.Nil) = pure $ List (x : List.Nil)
evalCons (x : List xs : List.Nil) = pure $ List (x : xs)
evalCons (_ : x : _) = throwError $ TypeMismatch "collection" x
evalCons ba = throwError $ NumArgs 2 ba

eqv :: PrimFn WFF
eqv (Atom p : Atom q : List.Nil) = dumbEq p q
eqv (String s : String s' : List.Nil) = dumbEq s s'
eqv (Integer n : Integer m : List.Nil) = dumbEq n m
eqv (Bool p : Bool q : List.Nil) = dumbEq p q
eqv (List xs : List ys : List.Nil) =
  allEq xs ys
eqv _ = throwError $ Default "Cannot call eqv in this context"

dumbEq :: forall a. Eq a => a -> a -> ThrowsError WFF
dumbEq x y = pure $ Bool $ x == y

allEq :: (List.List WFF) ~~> (ThrowsError WFF)
allEq = allEq' true where
  allEq' acc List.Nil List.Nil = pure $ Bool acc
  allEq' _ (_ : _) List.Nil = pure $ Bool false
  allEq' _ List.Nil (_ : _) = pure $ Bool false
  allEq' acc (x : xs) (y : ys) =
    eqv (x : y : List.Nil) >>= case _ of
                                    Bool b -> allEq' (acc && b) xs ys
                                    _ -> pure $ Bool false
