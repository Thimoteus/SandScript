module Lang.TypeChecker.TypeError where

import Prelude

import Lang.TypeChecker.Types (Type, showType)

data TypeError
  = OccursCheck String Type
  | TypesDoNotUnify Type Type
  | UnboundVariable String

instance showTypeError :: Show TypeError where
  show (OccursCheck s t) = "Occurs check: " <> s <> " in " <> showType t
  show (TypesDoNotUnify t1 t2) = "Types do not unify: " <> showType t1 <> " with " <> showType t2
  show (UnboundVariable s) = "Unbound variable: " <> s
