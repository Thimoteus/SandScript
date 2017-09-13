module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.StrMap as Map
import Data.Tuple (Tuple(..))
import Lang.JavaScript (JSExpr, ifThenElse, jsArray, jsBind, jsIIFE, jsIdent, jsLambda, jsObject, jsbool, jsint, jsnum)
import Lang.JavaScript.Printer (pprintJS)
import PPrint (runPrinter)

objJS :: JSExpr
objJS = jsObject $ Map.fromFoldable [Tuple "a" (jsint 1), Tuple "b" (jsint 2)]

nestedObjJS :: JSExpr
nestedObjJS = jsObject $ Map.fromFoldable [Tuple "x" objJS, Tuple "y" (jsint 1)]

arrayJS :: JSExpr
arrayJS = jsArray [jsint 1, jsbool true, jsnum 4.5]

nestedArrayJS :: JSExpr
nestedArrayJS = jsArray [jsint 1, arrayJS, jsnum 0.0]

objInArrayJS :: JSExpr
objInArrayJS = jsArray [jsint 1, nestedArrayJS, nestedObjJS, jsbool false]

lambdaJS :: JSExpr
lambdaJS = jsLambda "x" $ jsLambda "y" $ jsIdent "x"

ifthenJS :: JSExpr
ifthenJS = ifThenElse (jsbool true) (jsbool true) (jsbool false)

binding :: JSExpr
binding = jsBind "myvar" lambdaJS

iife :: JSExpr
iife = jsIIFE (Map.fromFoldable [Tuple "x" objJS, Tuple "y" (jsint 1)]) (jsIdent "y")

logJSExpr :: JSExpr -> Eff (console :: CONSOLE) Unit
logJSExpr = log <<< runPrinter <<< pprintJS

main :: Eff (console :: CONSOLE) Unit
main = do
  log "Printing 1"
  logJSExpr $ jsint 1
  log "Printing object"
  logJSExpr objJS
  log "Printing nested object"
  logJSExpr nestedObjJS
  log "Printing array"
  logJSExpr arrayJS
  log "Printing nested array"
  logJSExpr $ nestedArrayJS
  log "Printing array with nested array and nested object"
  logJSExpr objInArrayJS
  log "Printing lambda"
  logJSExpr lambdaJS
  log "Printing ifThenElse"
  logJSExpr ifthenJS
  log "Printing binding"
  logJSExpr binding
  log "Printing IIFE"
  logJSExpr iife
