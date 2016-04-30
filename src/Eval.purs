module SandScript.Eval where

import Prelude

import Control.Bind (join)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (get, withStateT, put)

import Data.Foldable (foldl)
import Data.List as List
import Data.StrMap as Map
import Data.Maybe (Maybe(..), maybe')
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))

import SandScript.AST (WFF(..), LangError(..), ThrowsError, PrimFn)
import SandScript.Env (Env, State, runState, liftState, defineVar, getVar, liftThrows, bindVars)
import SandScript.Parser (read)

infixr 5 List.Cons as :

readEvalString :: forall m. Monad m => String -> State m (Tuple Env WFF)
readEvalString s = do
  wff <- liftState (read s) >>= eval
  env <- get
  pure (Tuple env wff)

runComputation :: forall m. Monad m => Env -> String -> m (ThrowsError (Tuple Env WFF))
runComputation env s = runState env $ readEvalString s

eval :: forall m. Monad m => WFF -> State m WFF
eval v@(String _) = pure v
eval n@(Integer _) = pure n
eval b@(Bool _) = pure b
eval (Atom i) = getVar i
eval (List (Atom "quote" : q : List.Nil)) = pure q
eval (List (Atom "if" : pred : conseq : alt : List.Nil)) = evalIf pred conseq alt
eval (List (Atom "def" : Atom v : form : List.Nil)) = eval form >>= defineVar v
eval (List (Atom "def" : List (Atom var : params) : body)) = do
  env <- get
  defn <- makeNormalFunc env params body
  defineVar var defn
eval (List (Atom "lambda" : List params : body)) = do
  env <- get
  makeNormalFunc env params body
eval (List (Atom "λ" : List params : body)) = do
  env <- get
  makeNormalFunc env params body
eval (List (f : args)) = do
  func <- eval f
  argv <- traverse eval args
  mapply func argv
eval bsf = throwError $ BadSpecialForm "Unrecognized special form" bsf

mapply :: forall m. Monad m => WFF -> List.List WFF -> State m WFF
mapply (PrimitiveFunc f) xs = liftThrows $ f xs
mapply (Func {params, vararg, body, closure}) args =
  let numParams = List.length params
      remainingArgs = List.drop numParams args
      bindVarArgs (Just name) = bindVars $ Tuple name (List remainingArgs) : List.Nil
      bindVarArgs _ = id
      getLast = maybe' (\_ -> throwError $ NotFunction "Not a function" "Needs a nonempty body") pure <<< List.last
      modifiedEval cls = withStateT (Map.union cls) <<< eval
      evalEach cls = traverse (modifiedEval cls) body
      evalBody cls = join $ getLast <$> evalEach cls
   in if numParams /= List.length args
         then throwError $ NumArgs numParams args
         else do
           env :: Env <- get
           result <- evalBody $ bindVarArgs vararg $ bindVars (List.zip params args) closure
           put env
           pure result
mapply _ _ = throwError $ NotFunction "Not a function" "This should be impossible"

makeFunc :: forall m. Monad m => Maybe String -> Env -> List.List WFF -> List.List WFF -> State m WFF
makeFunc vararg closure params body =
  let f = Func { params: (map show params), vararg, body, closure }
   in pure f

makeNormalFunc :: forall m . Monad m => Env -> List.List WFF -> List.List WFF -> State m WFF
makeNormalFunc = makeFunc Nothing

makeVarArgs :: forall m b. (Monad m , Show b) => b -> Env -> List.List WFF -> List.List WFF -> State m WFF
makeVarArgs = makeFunc <<< Just <<< show

primitiveFuncs :: Env
primitiveFuncs = map PrimitiveFunc primitives

data Op = Add | Sub | Mul | Div | Mod

primitives :: Map.StrMap (PrimFn WFF)
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

evalIf :: forall m. Monad m => WFF -> WFF -> WFF -> State m WFF
evalIf p c a =
  eval p >>= case _ of
                  (Bool b) -> if b then eval c else eval a
                  x -> throwError $ TypeMismatch "bool" x

evalHead :: PrimFn WFF
evalHead ((List (x : _)) : List.Nil) = pure x
evalHead ((DotList (x : _) _) : List.Nil) = pure x
evalHead (x : List.Nil) = throwError $ TypeMismatch "list" x
evalHead xs = throwError $ NumArgs 1 xs

evalTail :: PrimFn WFF
evalTail ((List (_ : xs)) : List.Nil) = pure $ List xs
evalTail ((DotList (x : List.Nil) _) : List.Nil) = pure x
evalTail ((DotList (_ : xs) x) : List.Nil) = pure $ DotList xs x
evalTail (ba : List.Nil) = throwError $ TypeMismatch "pair" ba
evalTail xs = throwError $ NumArgs 1 xs

evalCons :: PrimFn WFF
evalCons (x : List.Nil) = pure $ List (x : List.Nil)
evalCons (x : List xs : List.Nil) = pure $ List (x : xs)
evalCons (x : DotList xs y : List.Nil) = pure $ DotList (x : xs) y
evalCons (_ : x : _) = throwError $ TypeMismatch "collection" x
evalCons ba = throwError $ NumArgs 2 ba

eqv :: PrimFn WFF
eqv (Atom p : Atom q : List.Nil) = dumbEq p q
eqv (String s : String s' : List.Nil) = dumbEq s s'
eqv (Integer n : Integer m : List.Nil) = dumbEq n m
eqv (Bool p : Bool q : List.Nil) = dumbEq p q
eqv (DotList xs x : DotList ys y : List.Nil) =
  eqv $ (List (x : xs)) : (List (y : ys)) : List.Nil
eqv (List xs : List ys : List.Nil) =
  allEq xs ys
eqv _ = throwError $ Default "Cannot call eqv in this context"

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

