module Lang.Haskell98.Kinds where

import Prelude

data Kind = Star | Kfun Kind Kind
derive instance eqKind :: Eq Kind
derive instance ordKind :: Ord Kind

infixr 9 Kfun as *->*

class HasKind t where
  kind :: t -> Kind
