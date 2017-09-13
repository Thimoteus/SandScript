module Lang.TypeChecker.Types where

import Prelude

import Data.Functor.Mu (Mu(..))
import Data.StrMap (StrMap)

data TypeF r
  = TVar String
  | TLit TLit
  | TFun r r

data TLit
  = TInt
  | TBool
  | TNumber
  | TChar
  | TString

derive instance eqTLit :: Eq TLit
derive instance ordTLit :: Ord TLit

derive instance eqType :: Eq r => Eq (TypeF r)
derive instance ordType :: Ord r => Ord (TypeF r)

type Type = Mu TypeF

tvar :: String -> Type
tvar = In <<< TVar

tint :: Type
tint = In $ TLit TInt

tbool :: Type
tbool = In $ TLit TBool

tnumber :: Type
tnumber = In $ TLit TNumber

tchar :: Type
tchar = In $ TLit TChar

tstring :: Type
tstring = In $ TLit TString

tfun :: Type -> Type -> Type
tfun t1 t2 = In (TFun t1 t2)

data Scheme = Scheme (Array String) Type

type Subst = StrMap Type
