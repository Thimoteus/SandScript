module SandScript.Eval where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.List
import qualified Data.List.Unsafe as U

import Control.Apply ((*>))
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Bind ((=<<))
import Control.Monad.Eff.Class (liftEff)

import Node.FS
import Node.FS.Sync
import Node.Buffer (toString)
import Node.Encoding

import SandScript.Types
import SandScript.Errors
import SandScript.Util
import SandScript.Parser
import SandScript.Variables
import SandScript.Eval.Primitives

eval :: Env -> LispVal -> EffThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Int _) = return val
eval env val@(Frac (Tuple _ _)) = return $ simplifyFrac val
eval env val@(Float _) = return val
eval env val@(Complex _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List (Cons (Atom "quote") (Cons val Nil))) = return val
eval env (List (Cons (Atom "if") (Cons pred (Cons (conseq) (Cons alt Nil))))) = evalIf env pred conseq alt
eval env (List (Cons (Atom "set") (Cons (Atom var) (Cons form Nil)))) = eval env form >>= setVar env var
eval env (List (Cons (Atom "def") (Cons (Atom var) (Cons form Nil)))) = eval env form >>= defineVar env var
eval env (List (Cons (Atom "load") (Cons (String filename) Nil))) = load filename >>= map U.last <<< traverse (eval env)
eval env (List (Cons (Atom "def") (Cons (List (Cons (Atom var) params)) body))) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Cons (Atom "def") (Cons (DottedList (Cons (Atom var) params) varargs) body))) = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Cons (Atom "lambda") (Cons (List params) body))) = makeNormalFunc env params body
eval env (List (Cons (Atom "lambda") (Cons (DottedList params varargs) body))) = makeVarArgs varargs env params body
eval env (List (Cons (Atom "lambda") (Cons varargs@(Atom _) body))) = makeVarArgs varargs env Nil body
eval env (List (Cons (Atom "case") args)) = evalCase env args
eval env (List (Cons (Atom "cond") args)) = evalCond env args
eval env (List (Cons function args)) = do
  func <- eval env function
  argVals <- traverse (eval env) args
  fapply func argVals
eval env badform = throwError $ BadSpecialForm "Unrecognized special form" badform

fapply :: LispVal -> List LispVal -> EffThrowsError LispVal
fapply (PrimitiveFunc func) args = liftThrows $ func args
fapply (Func { params = params, varargs = varargs, body = body, closure = closure }) args =
  if (length params /= length args) && isNothing varargs
     then throwError $ NumArgs (length params) args
     else (liftEff $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody where
       remainingArgs = drop (length params) args
       evalBody env = map U.last $ traverse (eval env) body
       bindVarArgs arg env = case arg of
                                  Just argName -> liftEff $ bindVars env (Tuple argName (List remainingArgs) : Nil)
                                  Nothing -> return env
fapply (EffFunc func) args = func args

makeFunc :: Maybe String -> Env -> List LispVal -> List LispVal -> EffThrowsError LispVal
makeFunc varargs env params body = return $ Func { params: (map show params), varargs: varargs, body: body, closure: env }
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc <<< Just <<< (show :: LispVal -> String)

-- conditional stuff
evalIf :: Env -> LispVal -> LispVal -> LispVal -> EffThrowsError LispVal
evalIf env pred conseq alt = do
  result <- eval env pred
  case result of
       Bool false -> eval env alt
       Bool true -> eval env conseq
       notBool -> throwError $ TypeMismatch "bool" notBool

evalCase :: Env -> List LispVal -> EffThrowsError LispVal
evalCase env (Cons x xs) = ecase env x xs
evalCase _ _ = throwError $ NumArgs 1 Nil

ecase :: Env -> LispVal -> List LispVal -> EffThrowsError LispVal
ecase _ _ Nil = throwError PatternFail
ecase env _ (Cons (List (Cons (Atom "else") (Cons val Nil))) Nil) = eval env val
ecase env key (Cons (List (Cons (List datums) (Cons val Nil))) rest) = do
  evaldKey <- eval env key
  if evaldKey `elem` datums
     then eval env val
     else ecase env key rest

evalCond :: Env -> List LispVal -> EffThrowsError LispVal
evalCond env (Cons (List (Cons (Atom "else") (Cons expr Nil))) Nil) = eval env expr
evalCond env (Cons (List (Cons test@(List _) (Cons expr Nil))) rest) = condChecker env test expr rest
evalCond env (Cons (List (Cons test@(List _) Nil)) rest) = condChecker env test test rest
evalCond _ _ = throwError ConditionalFail

condChecker :: Env -> LispVal -> LispVal -> List LispVal -> EffThrowsError LispVal
condChecker env test expr rest = do
  result <- eval env test
  case result of
       Bool true -> eval env expr
       _ -> evalCond env rest

-- EffPrimitives

effPrimitives :: List (Tuple String ((List LispVal) -> EffThrowsError LispVal))
effPrimitives = "apply" & applyProc
              : "open-input-file" & makePort R
              : "open-output-file" & makePort W
              : "close-input-port" & closePort
              : "read-contents" & readContents
              : "read-all" & readAll
              : "close-output-port" & closePort
              : Nil

applyProc :: List LispVal -> EffThrowsError LispVal
applyProc (Cons func (Cons (List args) Nil)) = fapply func args
applyProc (Cons func args) = fapply func args

makePort :: FileFlags -> List LispVal -> EffThrowsError LispVal
makePort flag (Cons (String filename) Nil) = liftEff $ Port <$> fdOpen filename flag Nothing

closePort :: List LispVal -> EffThrowsError LispVal
closePort (Cons (Port port) Nil) = liftEff $ fdClose port *> (return $ Bool true)
closePort _ = return $ Bool false

readContents :: List LispVal -> EffThrowsError LispVal
readContents (Cons (String filename) Nil) = liftEff $ (String <<< toString ASCII) <$> readFile filename

load :: String -> EffThrowsError (List LispVal)
load filename = (liftEff $ toString ASCII <$> readFile filename) >>= (liftThrows <<< readExprList)

readAll :: List LispVal -> EffThrowsError LispVal
readAll (Cons (String filename) Nil) = map List $ load filename

primitiveBindings :: LispF Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc' PrimitiveFunc) primitives ++ map (makeFunc' EffFunc) effPrimitives)
  where
  makeFunc' :: forall a. (a -> LispVal) -> Tuple String a -> Tuple String LispVal
  makeFunc' constructor (Tuple var func) = Tuple var (constructor func)
