module SandScript.Eval where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (withStateT, get)

import Data.List as List
import Data.Maybe (fromMaybe')
import Data.StrMap as Map
import Data.Foldable (class Foldable, any, or)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))

import SandScript.AST (WFF(..), LangError(..), ThrowsError)
import SandScript.Env (Env, State, runState, liftState, defineVar, getVar, liftThrows, bindVars, retainState)
import SandScript.Parser (read, readFile)
import SandScript.Primitives (primitives)

infixr 5 List.Cons as :

readEvalMany :: forall m. Monad m => String -> State m (Tuple Env WFF)
readEvalMany file = do
  wffs <- liftState $ readFile file
  evaleds <- traverse eval wffs
  finalenv <- get
  let lastwff = fromMaybe' (\_ -> String "") $ List.last evaleds
  pure $ Tuple finalenv lastwff

readEvalOne :: forall m. Monad m => String -> State m (Tuple Env WFF)
readEvalOne s = do
  wff <- liftState (read s) >>= eval
  env <- get
  pure (Tuple env wff)

runComputation :: forall m. Monad m => Env -> String -> m (ThrowsError (Tuple Env WFF))
runComputation env s = runState env $ readEvalOne s

runComputations :: forall m. Monad m => Env -> String -> m (ThrowsError (Tuple Env WFF))
runComputations env = runState env <<< readEvalMany

eval :: forall m. Monad m => WFF -> State m WFF
eval v@(String _) = pure v
eval n@(Integer _) = pure n
eval r@(Float _) = pure r
eval b@(Bool _) = pure b
eval v@(Vector _) = pure v
eval (Atom i) = getVar i
eval (List (Atom "quote" : q : List.Nil)) = pure q
eval (List (Atom "if" : pred : conseq : alt : List.Nil)) = evalIf pred conseq alt
eval (List (Atom "defn" : Atom v : Vector params : body : List.Nil)) = do
  env <- get
  defn <- makeFunc env params body
  defineVar v defn
eval (List (Atom "def" : Atom v : form : List.Nil)) = eval form >>= defineVar v
eval (List (Atom "def" : List (Atom var : params) : body : List.Nil)) = do
  env <- get
  defn <- makeFunc env params body
  defineVar var defn
eval (List (Atom "lambda" : List params : body : List.Nil)) = do
  env <- get
  makeFunc env params body
eval (List (Atom "λ" : List params : body : List.Nil)) = do
  env <- get
  makeFunc env params body
eval (List (f : args)) = do
  func <- eval f
  argv <- traverse eval args
  mapply func argv
eval bsf = throwError $ BadSpecialForm "Unrecognized special form" bsf

mapply :: forall m. Monad m => WFF -> List.List WFF -> State m WFF
mapply (PrimitiveFunc f) xs = liftThrows $ f xs
mapply (Func {params, body, closure}) args =
  let numParams = List.length params
      remainingArgs = List.drop numParams args
      modifiedEval cls = withStateT (Map.union cls) <<< eval
      evalBody cls = modifiedEval cls body
   in if numParams /= List.length args
         then throwError $ NumArgs numParams args
         else retainState $ evalBody $ bindVars (List.zip params args) closure
mapply nf _ = throwError $ NotFunction "Expecting a function" (show nf <> " is not a function")

makeFunc :: forall f m. (Functor f, Foldable f, Monad m) => Env -> f WFF -> WFF -> State m WFF
makeFunc closure params body =
  let f = Func { params: List.fromFoldable (map show params), body, closure }
      isn'tAtom (Atom _) = false
      isn'tAtom _ = true
      someNonAtom = any isn'tAtom
   in if or [someNonAtom params]
         then throwError $ NotFunction "Not a function" "Function was ill defined"
         else pure f

{-- makeNormalFunc :: forall m . Monad m => Env -> List.List WFF -> List.List WFF -> State m WFF --}
{-- makeNormalFunc = makeFunc Nothing --}

{-- makeVarArgs :: forall m b. (Monad m , Show b) => b -> Env -> List.List WFF -> List.List WFF -> State m WFF --}
{-- makeVarArgs = makeFunc <<< Just <<< show --}

primitiveFuncs :: Env
primitiveFuncs = map PrimitiveFunc primitives

evalIf :: forall m. Monad m => WFF -> WFF -> WFF -> State m WFF
evalIf p c a =
  eval p >>= case _ of
                  (Bool b) -> if b then eval c else eval a
                  x -> throwError $ TypeMismatch "bool" x
