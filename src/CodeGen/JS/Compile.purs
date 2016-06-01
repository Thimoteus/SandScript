module CodeGen.JS.Compile where

import Prelude
import Syntax (WFF(..))
import CodeGen.JS.JS (JSExpr(..), JSBinop(..), JSUnop(..), until)

import Data.List as List
import Data.Set as Set
import Data.String as String
import Data.List ((..))
import Data.Array (elemIndex)
import Data.Either (Either(..))
import Data.Maybe (isJust)
import Data.Traversable (class Traversable, traverse)

infixr 5 List.Cons as :

elem :: forall a. Eq a => a -> Array a -> Boolean
elem x = isJust <<< elemIndex x

range :: Int -> Int -> List.List Int
range low up | low <= up = low .. up
range low up = List.Nil

infix 8 range as ...

data CompileError = NotAtom WFF
                  | UnexpectedForm WFF
                  | BadArgument Int Int

instance showCompileError :: Show CompileError where
  show (NotAtom e) = show e <> " is not an atom, but was used where only atoms are allowed"
  show (UnexpectedForm e) = "Bad special form: " <> show e
  show (BadArgument exp fd) = "Wrong arguments: Expecting " <> show exp <> " but found " <> show fd

type Expect a = Either CompileError a

runAtoms :: forall t. Traversable t => t WFF -> Expect (t String)
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
  List (Atom "quote" : q : List.Nil) -> compile q
  List (Atom "module" : Atom name : Vector exports : List.Nil) -> Export <$> runAtoms exports
  List (Atom "if" : pred : conseq : alt : List.Nil) ->
    let cond = JSIf <$> compile pred <*> (JSReturn <$> compile conseq) <*> (JSReturn <$> compile alt)
     in flip JSFunCall List.Nil <<< JSLambda List.Nil <$> cond
  {-- List (Atom "defn" : Atom v : Vector params : body : List.Nil) -> do --}
  {--   args <- runAtoms $ List.fromFoldable params --}
  {--   b <- compile body --}
  {--   pure $ JSAssign v $ JSLambda args $ JSReturn b --}
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
compileFun (Atom "binop") (Atom js : args) = case args of
  (_ : _ : _) -> JSBinop (Binop js) <$> traverse compile args
  _ -> Left $ BadArgument 2 (List.length args)
compileFun (Atom "unop") (Atom js : wff : List.Nil) = JSUnop (Unop js) <$> compile wff
compileFun (Atom "ffi") (String s : List.Nil) = pure $ FFI s
compileFun (Atom "ffi-fun") (String ffistring : List.Nil) =
  let s = interpolate ffistring
      args = Set.toList $ Set.delete "" s.args
   in pure $ JSLambda args $ JSReturn $ FFI s.ffi
compileFun f args = JSFunCall <$> compile f <*> traverse compile args

interpolate :: String -> { args :: Set.Set String, ffi :: String }
interpolate string = until recEq takeOne {args: Set.empty, ffi: string}
  where
  recEq o1 o2 = o1.args == o2.args && o1.ffi == o2.ffi
  takeOne o =
    let prefix = String.takeWhile (_ /= '{') o.ffi
        var = String.takeWhile (_ /= '}') $ String.drop (String.length prefix + 2) o.ffi
        suffix = String.drop (String.length prefix + String.length var + 4) o.ffi
        args = Set.insert var o.args
        ffi = prefix <> var <> suffix
     in {args, ffi}
