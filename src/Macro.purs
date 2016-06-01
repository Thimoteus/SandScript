module Macro where

import Prelude
import Syntax (WFF(..))

import Data.List as List
import Data.Either (Either(..))

infixr 5 List.Cons as :

data MacroError = MalformedMacro

instance showMacroError :: Show MacroError where
  show MalformedMacro = "Malformed macro"

type Expect a = Either MacroError a

data Macro = CompileMacro CompileMacro

data CompileMacro = Binop String String
                  | Unop String String

splitWFFs :: List.List WFF -> { supercombinators :: List.List WFF, macros :: List.List Macro }
splitWFFs = splitWFFs' { supercombinators: List.Nil, macros: List.Nil }
  where
    splitWFFs' acc (x : xs) | isMacro x = case macro x of
      Right m -> splitWFFs' (acc {macros = m : acc.macros}) xs
      Left _ -> splitWFFs' acc xs
    splitWFFs' acc (x : xs) = splitWFFs' (acc {supercombinators = x : acc.supercombinators}) xs
    splitWFFs' acc _ = acc

isMacro :: WFF -> Boolean
isMacro (List (Atom "macro" : _)) = true
isMacro _ = false

applyMacro :: List.List WFF -> Macro -> List.List WFF
applyMacro xs = case _ of
  CompileMacro (Binop b name) -> compileBinop b name <$> xs
  CompileMacro (Unop b name) -> compileUnop b name <$> xs
  _ -> xs

compileBinop :: String -> String -> WFF -> WFF
compileBinop js alias e = case e of
  List (Atom f : args) ->
    if f == alias
       then List (Atom "binop" : Atom js : (compileBinop js alias <$> args))
       else List (Atom f : (compileBinop js alias <$> args))
  _ -> e

compileUnop :: String -> String -> WFF -> WFF
compileUnop js alias e = case e of
  List (Atom f : wff : List.Nil) ->
    if f == alias
       then List (Atom "unop" : Atom js : (compileUnop js alias wff) : List.Nil)
       else List (Atom f : compileUnop js alias wff : List.Nil)
  List (Atom f : args) -> List (Atom f : map (compileUnop js alias) args)
  _ -> e

macro :: WFF -> Expect Macro
macro (List (Atom "macro" : Atom "JS#binop" : String binop : Atom ssname : List.Nil)) =
  pure $ CompileMacro $ Binop binop ssname
macro (List (Atom "macro" : Atom "JS#unop" : String unop : Atom ssname : List.Nil)) =
  pure $ CompileMacro $ Unop unop ssname
macro _ = Left MalformedMacro

applyAllMacros :: { supercombinators :: List.List WFF, macros :: List.List Macro } -> List.List WFF
applyAllMacros o = case o.macros of
  (m : ms) ->
    applyAllMacros {macros: ms, supercombinators: applyMacro o.supercombinators m}
  _ -> o.supercombinators

macroize :: List.List WFF -> List.List WFF
macroize = applyAllMacros <<< splitWFFs
