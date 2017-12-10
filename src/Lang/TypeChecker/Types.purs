module Lang.TypeChecker.Types where

import Prelude

import Data.Functor.Mu (Mu(..))
import Data.StrMap (StrMap)
import Matryoshka (cata)

data TypeF r
  = TVar String
  | TCon String
  | TFun r r
  | TArr r

derive instance eqTLit :: Eq TLit
derive instance ordTLit :: Ord TLit

derive instance eqType :: Eq r => Eq (TypeF r)
derive instance ordType :: Ord r => Ord (TypeF r)
derive instance functorTypeF :: Functor TypeF

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

tarr :: Type -> Type
tarr t = In (TArr t)

showType :: Type -> String
showType = cata showType' where
  showType' :: TypeF String -> String
  showType' (TVar s) = s
  showType' (TLit l) = showLit l
  showType' (TFun s1 s2) = s1 <> " -> " <> s2
  showType' (TArr t) = "[" <> t <> "]"

showLit :: TLit -> String
showLit TInt = "Int"
showLit TBool = "Bool"
showLit TNumber = "Float"
showLit TChar = "Char"
showLit TString = "String"

data Scheme = Scheme (Array String) Type

type Subst = StrMap Type
