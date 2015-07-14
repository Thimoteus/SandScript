module SandScript.Eval where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Array hiding (cons)
import qualified Data.Array.Unsafe as U

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
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
-- variables
eval env (Atom id) = getVar env id
-- special list stuff
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = evalIf env pred conseq alt
eval env (List [Atom "set", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "def", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "load", String filename]) = load filename >>= map U.last <<< traverse (eval env)
-- general list stuff
eval env (List ls)
  | ls !! 0 == Just (Atom "def") =
    case uncons (drop 1 ls) of
         Just { head = List xs, tail = body } -> case uncons xs of
                                                      Just { head = Atom var, tail = params } -> makeNormalFunc env params body >>= defineVar env var
                                                      _ -> throwError $ NotFunction "Did not recognize function" (show $ List xs)
         Just { head = DottedList xs varargs, tail = body } -> case uncons xs of
                                                                    Just { head = Atom var, tail = params } -> makeVarArgs varargs env params body >>= defineVar env var
                                                                    _ -> throwError $ NotFunction "Did not recognize function" (show $ List xs)
         _ -> throwError $ NotFunction "Failed `def` parse" (show $ List ls)
  | ls !! 0 == Just (Atom "lambda") =
    case uncons (drop 1 ls) of
         Just { head = List params, tail = body } -> makeNormalFunc env params body
         Just { head = DottedList params varargs, tail = body } -> makeVarArgs varargs env params body
         Just { head = varargs@(Atom _), tail = body } -> makeVarArgs varargs env [] body
         _ -> throwError $ NotFunction "Failed `lambda` parse" (show $ List ls)
  | otherwise = case uncons ls of
                     -- Just { head = Atom "case", tail = args } -> evalCase env args
                     -- Just { head = Atom "cond", tail = args } -> evalCond env args
                     Just { head = function, tail = args } -> do
                       func <- eval env function
                       argVals <- traverse (eval env) args
                       fapply func argVals
                     _ -> throwError $ NotFunction "Did not recognize function" (show $ List ls)
eval env badform = throwError $ BadSpecialForm "Unrecognized special form" badform

fapply :: LispVal -> Array LispVal -> EffThrowsError LispVal
fapply (PrimitiveFunc func) args = liftThrows $ func args
fapply (Func { params = params, varargs = varargs, body = body, closure = closure }) args =
  if (length params /= length args) && isNothing varargs
     then throwError $ NumArgs (length params) args
     else (liftEff $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody where
       remainingArgs = drop (length params) args
       evalBody env = map U.last $ traverse (eval env) body
       bindVarArgs arg env = case arg of
                                  Just argName -> liftEff $ bindVars env [Tuple argName (List remainingArgs)]
                                  Nothing -> return env

makeFunc :: Maybe String -> Env -> Array LispVal -> Array LispVal -> EffThrowsError LispVal
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

-- EffPrimitives

effPrimitives :: Array (Tuple String ((Array LispVal) -> EffThrowsError LispVal))
effPrimitives = [ "apply" & applyProc
                , "open-input-file" & makePort R
                , "open-output-file" & makePort W
                , "close-input-port" & closePort
                , "read-contents" & readContents
                , "read-all" & readAll
                , "close-output-port" & closePort ]

applyProc :: Array LispVal -> EffThrowsError LispVal
applyProc [func, List args] = fapply func args
applyProc arr | length arr > 0 = fapply (U.head arr) (U.tail arr)

makePort :: FileFlags -> Array LispVal -> EffThrowsError LispVal
makePort flag [String filename] = liftEff $ Port <$> fdOpen filename flag Nothing

closePort :: Array LispVal -> EffThrowsError LispVal
closePort [Port port] = liftEff $ fdClose port >> (return $ Bool true)
closePort _ = return $ Bool false

readContents :: Array LispVal -> EffThrowsError LispVal
readContents [String filename] = liftEff $ (String <<< toString ASCII) <$> readFile filename

load :: String -> EffThrowsError (Array LispVal)
load filename = (liftEff $ toString ASCII <$> readFile filename) >>= (liftThrows <<< readExprList)

readAll :: Array LispVal -> EffThrowsError LispVal
readAll [String filename] = map List $ load filename

primitiveBindings :: LispF Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc' PrimitiveFunc) primitives ++ map (makeFunc' EffFunc) effPrimitives)
  where
  makeFunc' :: forall a. (a -> LispVal) -> Tuple String a -> Tuple String LispVal
  makeFunc' constructor (Tuple var func) = Tuple var (constructor func)

{--
evalCase :: forall r. Env -> Array LispVal -> EffThrowsError r LispVal
evalCase env as = case uncons as of
                       Just xs -> ecase env xs.head xs.tail
                       Nothing -> throwError $ NumArgs 1 []

ecase :: forall r. Env -> LispVal -> Array LispVal -> EffThrowsError r LispVal
ecase _ _ [] = throwError PatternFail
ecase env _ [List [Atom "else", val]] = eval env val
ecase env key clauses = case uncons clauses of
                             Just { head = List [List datums, val], tail = rest } -> if eqv' env key datums
                                                                                        then return val
                                                                                        else ecase env key rest
                             Just xs -> throwError $ BadSpecialForm "Incorrect `case` syntax" xs.head
                             Nothing -> throwError $ NumArgs 2 clauses

eqv' :: Env -> LispVal -> Array LispVal -> Boolean
eqv' env k ds = let pairs :: Array (Array LispVal)
                    pairs = zipWith (\ a b -> [a, b]) (replicate (length ds) k) ds
                    evaled :: forall r. Array (EffThrowsError r (Array LispVal))
                    evaled = map (traverse (eval env)) pairs
                    flipped :: forall r. EffThrowsError r (Array (Array LispVal))
                    flipped = sequence evaled
                    ran :: forall r. (REff r) (Either LispError (Array (Array LispVal) ))
                    ran = runErrorT flipped
                 in case flipped of
                         Left _ -> false
                         Right evald -> either (const false) (\ bs -> or $ map (\ (Bool b) -> b) bs) (traverse eqv evald)
evalCond :: forall r. Env -> Array LispVal -> EffThrowsError r LispVal
evalCond env clauses = case uncons clauses of
                            Just { head = List [Atom "else", expr],  tail = [] } -> eval env expr
                            Just { head = List [test@(List ts), expr], tail = rest } -> condChecker env test expr rest
                            Just { head = List [test@(List ts)], tail = rest } -> condChecker env test test rest
                            Just xs -> throwError $ BadSpecialForm "Incorrect `cond` syntax" xs.head
                            _ -> throwError ConditionalFail

condChecker :: forall r. Env -> LispVal -> LispVal -> Array LispVal -> EffThrowsError r LispVal
condChecker env test expr rest
  | either (const false) (\ (Bool b) -> b) (eval test) = eval expr -- TODO: (eval test) !:: Either _ _, but ErorrT _ _ _
  | otherwise = evalCond env rest
  --}
