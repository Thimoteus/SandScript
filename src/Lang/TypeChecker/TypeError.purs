module Lang.TypeChecker.TypeError where

import Lang.TypeChecker.Types (Type)

data TypeError
  = OccursCheck String Type
  | TypesDoNotUnify Type Type
  | UnboundVariable String
