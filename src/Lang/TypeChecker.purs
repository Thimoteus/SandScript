-- Taken from https://github.com/wh5a/Algorithm-W-Step-By-Step
module Lang.TypeChecker where

import Control.Monad.Error.Class (throwError)
import Control.Monad.ErrorState (EState)
import Control.Monad.ErrorState as ES
import Data.Array as Array
import Data.Functor.Mu (unroll)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.StrMap (StrMap)
import Data.StrMap as Map
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Lang.AST as AST
import Lang.TypeChecker.Class (class Types, ftv, apply)
import Lang.TypeChecker.TypeError (TypeError(..))
import Lang.TypeChecker.Types (TypeF(..), Type, Scheme(..), Subst, tvar, tnumber, tint, tbool, tstring, tchar, tfun, tarr)
import Prelude hiding (apply)

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = map (apply s1) s2 `Map.union` s1

infix 1 composeSubst as <.>

-- mappings from term variables to their type schemes
newtype Γ = Γ (StrMap Scheme)

instance typesΓ :: Types Γ where
  ftv (Γ env) = ftv (Map.values env)
  apply s (Γ env) = Γ (map (apply s) env)

-- Γ ∖ x removes the binding for x from Γ
remove :: Γ -> String -> Γ
remove (Γ env) var =  Γ (Map.delete var env)

-- abstracts a type over all type variables which are free in the type but not
-- in the environment, or in other word, adds ∀ x for each free x
generalize :: Γ -> Type -> Scheme
generalize env t = Scheme vars t where
  vars = Set.toUnfoldable $ ftv t `Set.difference` ftv env

type TI a = EState Int TypeError a

newTyVar :: String -> TI Type
newTyVar prefix = do
  s <- ES.get
  ES.put $ s + 1
  pure $ tvar $ prefix <> show s

-- replaces all bound variables in a type scheme with fresh type variables
-- IOW, universal elimination
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- traverse (\_ -> newTyVar "a") vars
  pure $ apply (Map.fromFoldable (Array.zip vars nvars)) t

-- attempts to bind a variable to a type and return the binding as a substitution
-- but won't bind a variable to itself. also performs the occurs check, which makes
-- sure the variable doesn't already occur as a free type variable in the type
-- or else we'd get circularity
varBind :: String -> Type -> TI Subst
varBind u t
  | TVar n <- unroll t
  , n == u = pure nullSubst
  | u `Set.member` ftv t = throwError $ OccursCheck u t
  | otherwise = pure $ Map.singleton u t

-- returns the most general unifier for two types: for t1 and t2, mgu(t1, t2)
-- is a substitution S with S(t1) ≡ S(t2).
mgu :: Type -> Type -> TI Subst
mgu t1 t2
  | TFun l r <- unroll t1
  , TFun l' r' <- unroll t2 = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure $ s1 <.> s2
  | TVar u <- unroll t1 = varBind u t2
  | TVar u <- unroll t2 = varBind u t1
  | TLit l <- unroll t1
  , TLit r <- unroll t2
  , l == r = pure nullSubst
  | TArr l <- unroll t1
  , TArr r <- unroll t2 = mgu l r
  | otherwise = throwError $ TypesDoNotUnify t1 t2

tiLit :: AST.Literal -> TI (Tuple Subst Type)
tiLit = case _ of
  AST.String _ -> pure $ Tuple nullSubst tstring
  AST.Int _ -> pure $ Tuple nullSubst tint
  AST.Number _ -> pure $ Tuple nullSubst tnumber
  AST.Bool _ -> pure $ Tuple nullSubst tbool
  AST.Char _ -> pure $ Tuple nullSubst tchar

-- infers types for expressions. the Γ needs bindings for all free variables
-- in the expression. the returned Subst records the type constraints imposed on
-- type variables by the expression and the returned Type is the type of the
-- expression.
ti :: Γ -> AST.Expr -> TI (Tuple Subst Type)
ti e@(Γ env) expr
  | AST.Ident n <- unroll expr = case Map.lookup n env of
    Just σ -> do
      t <- instantiate σ
      pure $ Tuple nullSubst t
    _ -> throwError $ UnboundVariable n
  | AST.Lit l <- unroll expr = tiLit l
  | AST.Lambda n e' <- unroll expr = do
    tv <- newTyVar "t"
    let Γ env' = remove e n
        env'' = Γ (env' `Map.union` (Map.singleton n (Scheme [] tv)))
    Tuple s1 t1 <- ti env'' e'
    pure $ Tuple s1 (tfun (apply s1 tv) t1)
  | AST.LetIn x e1 e2 <- unroll expr = do
    Tuple s1 t1 <- ti e e1
    let Γ env' = remove e x
        t' = generalize (apply s1 e) t1
        env'' = Γ (Map.insert x t' env')
    Tuple s2 t2 <- ti (apply s1 env'') e2
    pure $ Tuple (s1 <.> s2) t2
  | AST.Apply e1 e2 <- unroll expr = do
    tv <- newTyVar "t"
    Tuple s1 t1 <- ti e e1
    Tuple s2 t2 <- ti (apply s1 e) e2
    s3 <- mgu (apply s2 t1) (tfun t2 tv)
    pure $ Tuple (s3 <.> s2 <.> s1) (apply s3 tv)
-- FIXME: this is probably wrong
  | AST.Array xs <- unroll expr =
    case Array.uncons xs of
         Nothing -> Tuple nullSubst <$> newTyVar "t"
         Just {head, tail} -> do
           Tuple s t <- ti e head
           pure $ Tuple s $ tarr t
ti _ _ = throwError $ TypesDoNotUnify tbool tbool

typeInference :: Γ -> AST.Expr -> TI Type
typeInference env e = do
  Tuple s t <- ti env e
  pure (apply s t)
