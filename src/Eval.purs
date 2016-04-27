module SandScript.Eval where

import Prelude

import SandScript.AST (WFF(..))
import SandScript.Errors (LangError(..), ThrowsError, Eval)

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
eval (List (Atom "if" : pred : conseq : alt : List.Nil)) = evalIf pred conseq alt
eval (List (Atom f : args)) = traverse eval args >>= apply f
eval bsf = throwError $ BadSpecialForm "Unrecognized special form" bsf

apply :: String -> Eval WFF
apply f args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" f)
  (_ $ args)
  (Map.lookup f primitives)

data Op = Add | Sub | Mul | Div | Mod

primitives :: Map.StrMap (Eval WFF)
primitives = Map.empty
           # Map.insert "+" (numericBinop Add)
           # Map.insert "-" (numericBinop Sub)
           # Map.insert "*" (numericBinop Mul)
           # Map.insert "/" (numericBinop Div)
           # Map.insert "mod" (numericBinop Mod)
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

numericBinop :: Op -> Eval WFF
numericBinop _ List.Nil = throwError $ NumArgs 2 List.Nil
numericBinop _ x@(_ : List.Nil) = throwError $ NumArgs 2 x
numericBinop Add xs = Integer <<< foldl add 0 <$> traverse unpackInt xs
numericBinop Mul xs = Integer <<< foldl mul 1 <$> traverse unpackInt xs
numericBinop Sub xs = Integer <$> numericFold sub 0 xs
numericBinop Div xs = Integer <$> numericFold div 1 xs
numericBinop Mod xs = Integer <$> numericFold mod 0 xs

numericFold :: (Int -> Int -> Int) -> Int -> Eval Int
numericFold op _ (x : xs) = do
  n <- unpackInt x
  ns <- traverse unpackInt xs
  pure $ foldl op n ns
numericFold op i _ = pure i

type Unpacker a = WFF -> ThrowsError a
type Operator a b = a -> a -> b
infixr 0 type Operator as ~~>

boolBinop :: forall a. Unpacker a -> a ~~> Boolean -> Eval WFF
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

boolBoolBinop :: Boolean ~~> Boolean -> Eval WFF
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: String ~~> Boolean -> Eval WFF
strBoolBinop = boolBinop unpackStr

intBoolBinop :: Int ~~> Boolean -> Eval WFF
intBoolBinop = boolBinop unpackInt

evalIf :: WFF -> WFF -> WFF -> ThrowsError WFF
evalIf p c a =
  eval p >>= case _ of
                  (Bool b) -> if b then eval c else eval a
                  x -> throwError $ TypeMismatch "bool" x

evalHead :: Eval WFF
evalHead ((List (x : _)) : List.Nil) = pure x
evalHead ((DotList (x : _) _) : List.Nil) = pure x
evalHead (x : List.Nil) = throwError $ TypeMismatch "list" x
evalHead xs = throwError $ NumArgs 1 xs

evalTail :: Eval WFF
evalTail ((List (_ : xs)) : List.Nil) = pure $ List xs
evalTail ((DotList (x : List.Nil) _) : List.Nil) = pure x
evalTail ((DotList (_ : xs) x) : List.Nil) = pure $ DotList xs x
evalTail (ba : List.Nil) = throwError $ TypeMismatch "pair" ba
evalTail xs = throwError $ NumArgs 1 xs

evalCons :: Eval WFF
evalCons (x : List.Nil) = pure $ List (x : List.Nil)
evalCons (x : List xs : List.Nil) = pure $ List (x : xs)
evalCons (x : DotList xs y : List.Nil) = pure $ DotList (x : xs) y
evalCons (_ : x : _) = throwError $ TypeMismatch "collection" x
evalCons ba = throwError $ NumArgs 2 ba

eqv :: Eval WFF
eqv (Atom p : Atom q : List.Nil) = dumbEq p q
eqv (String s : String s' : List.Nil) = dumbEq s s'
eqv (Integer n : Integer m : List.Nil) = dumbEq n m
eqv (Bool p : Bool q : List.Nil) = dumbEq p q
eqv (DotList xs x : DotList ys y : List.Nil) =
  eqv $ (List (x : xs)) : (List (y : ys)) : List.Nil
eqv (List xs : List ys : List.Nil) =
  allEq xs ys
eqv _ = throwError $ Default "fuck"

dumbEq :: forall a. Eq a => a -> a -> ThrowsError WFF
dumbEq x y = pure $ Bool $ x == y

allEq :: (List.List WFF) ~~> (ThrowsError WFF)
allEq = allEq' false where
  allEq' acc List.Nil List.Nil = pure $ Bool acc
  allEq' _ (_ : _) List.Nil = pure $ Bool false
  allEq' _ List.Nil (_ : _) = pure $ Bool false
  allEq' acc (x : xs) (y : ys) =
    eqv (x : y : List.Nil) >>= case _ of
                                    Bool b -> allEq' (acc || b) xs ys
                                    _ -> pure $ Bool false

