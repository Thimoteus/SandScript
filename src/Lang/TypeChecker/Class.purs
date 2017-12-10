module Lang.TypeChecker.Class where

import Prelude hiding (apply)

import Data.Foldable (foldr)
import Data.Functor.Mu (Mu, unroll)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.StrMap as Map
import Lang.TypeChecker.Types (Subst, TypeF(..), Scheme(..), tvar, tfun)

class Types a where
  ftv :: a -> Set String
  apply :: Subst -> a -> a

instance typesType :: Types (Mu TypeF) where
  ftv t
    | TVar n <- unroll t = Set.singleton n
    | TFun t1 t2 <- unroll t = ftv t1 `Set.union` ftv t2
    | otherwise = Set.empty
  apply s t
    | TVar n <- unroll t = fromMaybe (tvar n) $ Map.lookup n s
    | TFun t1 t2 <- unroll t = tfun (apply s t1) (apply s t2)
    | otherwise = t

instance typesScheme :: Types Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromFoldable vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance typesArr :: Types a => Types (Array a) where
  ftv l = foldr Set.union Set.empty (map ftv l)
  apply s = map (apply s)
