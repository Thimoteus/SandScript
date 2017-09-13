module Lang.JavaScript where

import Prelude

import Data.Functor.Mu (Mu(..))
import Data.StrMap (StrMap)
import Data.StrMap as Map
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Matryoshka (cata)

data JSLit
  = JSNum Number
  | JSInt Int
  | JSBool Boolean
  | JSString String
  | JSChar Char

data JSExprF r
  = JSLit JSLit
  | JSIdent String
  | JSObject (StrMap r)
  | JSArray (Array r)
  | JSLambda String r
  | IfThenElse r r r
  | JSBind String r
  | JSApply r r
  | IIFE (StrMap r) r

derive instance functorJSExprF :: Functor JSExprF

type JSExpr = Mu JSExprF

jslit :: JSLit -> JSExpr
jslit = In <<< JSLit

jsint :: Int -> JSExpr
jsint = jslit <<< JSInt

jsnum :: Number -> JSExpr
jsnum = jslit <<< JSNum

jsbool :: Boolean -> JSExpr
jsbool = jslit <<< JSBool

jsstring :: String -> JSExpr
jsstring = jslit <<< JSString

jschar :: Char -> JSExpr
jschar = jslit <<< JSChar

jsArray :: Array JSExpr -> JSExpr
jsArray = In <<< JSArray

jsObject :: StrMap JSExpr -> JSExpr
jsObject = In <<< JSObject

jsIdent :: String -> JSExpr
jsIdent = In <<< JSIdent

jsBind :: String -> JSExpr -> JSExpr
jsBind i e = In (JSBind i e)

ifThenElse :: JSExpr -> JSExpr -> JSExpr -> JSExpr
ifThenElse cond onTrue onFalse = In (IfThenElse cond onTrue onFalse)

jsLambda :: String -> JSExpr -> JSExpr
jsLambda arg body = In (JSLambda arg body)

jsApply :: JSExpr -> JSExpr -> JSExpr
jsApply f x = In (JSApply f x)

jsIIFE :: StrMap JSExpr -> JSExpr -> JSExpr
jsIIFE strmap return = In (IIFE strmap return)
