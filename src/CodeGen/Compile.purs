module SandScript.Compile where

import Prelude
import SandScript.AST (WFF(..))
import SandScript.JS (JSExpr(..), JSBinop(..))

import Data.List as List
import Data.Either (Either(..))

import Data.Traversable (traverse)

infixr 5 List.Cons as :

data CompileError = NotAtom WFF
                  | UnexpectedForm WFF

instance showCompileError :: Show CompileError where
  show (NotAtom e) = show e <> " is not an atom, but was used where only atoms are allowed"
  show (UnexpectedForm e) = "Bad special form: " <> show e

type Expect a = Either CompileError a

runAtoms :: List.List WFF -> Expect (List.List String)
runAtoms = traverse getAtom
  where
  getAtom (Atom a) = pure a
  getAtom e = Left $ NotAtom e

compile :: WFF -> Expect JSExpr
compile = case _ of
  String s -> pure $ JSString s
  Integer n -> pure $ JSInt n
  Float r -> pure $ JSNumber r
  Bool b -> pure $ JSBool b
  Atom s -> pure $ JSSymbol s
  Vector xs -> JSArray <$> traverse compile xs
  List (Atom "quote" : (List xs) : List.Nil) -> JSList <$> traverse compile xs
  List (Atom "quote" : q : List.Nil) -> compile q
  List (Atom "if" : pred : conseq : alt : List.Nil) ->
    let cond = JSIf <$> compile pred <*> (JSReturn <$> compile conseq) <*> (JSReturn <$> compile alt)
     in flip JSFunCall List.Nil <<< JSLambda List.Nil <$> cond
  List (Atom "defn" : Atom v : Vector params : body : List.Nil) -> do
    args <- runAtoms $ List.fromFoldable params
    b <- compile body
    pure $ JSAssign v $ JSLambda args $ JSReturn b
  List (Atom "def" : Atom v : form : List.Nil) -> JSAssign v <$> compile form
  List (Atom "def" : List (Atom v : params) : body : List.Nil) -> do
    args <- runAtoms params
    b <- compile body
    pure $ JSAssign v $ JSLambda args $ JSReturn b
  List (Atom "lambda" : List params : body : List.Nil) -> do
    args <- runAtoms params
    b <- compile body
    pure $ JSLambda args $ JSReturn b
  List (Atom "λ" : List params : body : List.Nil) -> do
    args <- runAtoms params
    b <- compile body
    pure $ JSLambda args $ JSReturn b
  List (f : args) -> compileFun f args
  e -> Left $ UnexpectedForm e

compileFun :: WFF -> List.List WFF -> Expect JSExpr
compileFun (Atom "+") (x : y : List.Nil) = JSBinop Add <$> compile x <*> compile y
compileFun (Atom "-") (x : y : List.Nil) = JSBinop Sub <$> compile x <*> compile y
compileFun (Atom "*") (x : y : List.Nil) = JSBinop Mul <$> compile x <*> compile y
compileFun (Atom "/") (x : y : List.Nil) = JSBinop Div <$> compile x <*> compile y
compileFun f args = JSFunCall <$> compile f <*> traverse compile args
