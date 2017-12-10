module Lang.Haskell98.Types where

import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Maybe.First (First(..))
import Data.Newtype (ala)
import Data.Tuple (fst, lookup, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Lang.Haskell98.Kinds (class HasKind, kind, Kind(..), (*->*))
import Lang.Haskell98.Util (Id)
import Prelude hiding (apply)

data Tyvar = Tyvar Id Kind
derive instance eqTyvar :: Eq Tyvar
derive instance ordTyvar :: Ord Tyvar
instance hasKindTyVar :: HasKind Tyvar where
  kind (Tyvar _ k) = k

-- NOTE: "A full Haskell compiler or interpreter might store additional information with
-- each type constant—such as the list of constructor functions for an algebraic
-- datatype—but such details are not needed during typechecking."
data Tycon = Tycon Id Kind
derive instance eqTycon :: Eq Tycon
instance hasKindTycon :: HasKind Tycon where
  kind (Tycon _ k) = k

data Type = TVar Tyvar | TCon Tycon | TAp Type Type | TGen Int
derive instance eqType :: Eq Type
instance hasKindType :: HasKind Type where
  kind = case _ of
    TVar t -> kind t
    TCon t -> kind t
    TAp t _ -> case kind t of
      Kfun _ k -> k
      _ -> Star
    _ -> Star

type Subst = Array (Tyvar /\ Type)

data TypeError
  = TypesDoNotUnify Type Type
  | MergeFail Subst Subst
  | OccursCheck Tyvar Type
  | KindMismatch Kind Kind
  | TypeMismatch Type Type

type Expect a = Either TypeError a

typesDoNotUnify :: forall a. Type -> Type -> Expect a
typesDoNotUnify t1 t2 = Left (TypesDoNotUnify t1 t2)

mergeFail :: forall a. Subst -> Subst -> Expect a
mergeFail t1 t2 = Left (MergeFail t1 t2)

occursCheck :: forall a. Tyvar -> Type -> Expect a
occursCheck t1 t2 = Left (OccursCheck t1 t2)

kindMismatch :: forall a. Kind -> Kind -> Expect a
kindMismatch t1 t2 = Left (KindMismatch t1 t2)

typeMismatch :: forall a. Type -> Type -> Expect a
typeMismatch t1 t2 = Left (TypeMismatch t1 t2)

tUnit = TCon (Tycon "()" Star) :: Type
tChar = TCon (Tycon "Char" Star) :: Type
tInt = TCon (Tycon "Int" Star) :: Type
tFloat = TCon (Tycon "Float" Star) :: Type
tList = TCon (Tycon "[]" (Star *->* Star)) :: Type
tArrow = TCon (Tycon "(->)" (Star *->* Star *->* Star)) :: Type
tTuple2 = TCon (Tycon "(,)" (Star *->* Star *->* Star)) :: Type

fn :: Type -> Type -> Type
fn a b = TAp (TAp tArrow a) b

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b

nullSubst :: Subst
nullSubst = []

tyvarToType :: Tyvar -> Type -> Subst
tyvarToType u t = [u /\ t]

infix 1 tyvarToType as +->

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> Array Tyvar

instance typesType :: Types Type where
  apply s (TVar u) = case lookup u s of
    Just t -> t
    _ -> TVar u
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply _ t = t
  tv (TVar u) = [u]
  tv (TAp l r) = tv l `Array.union` tv r
  tv _ = []

instance typesArray :: Types a => Types (Array a) where
  apply s = map (apply s)
  tv = Array.nub <<< Array.concat <<< map tv

-- This makes Subst's a category with id = nullSubst
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = s <> s1 where
  s = do
    u /\ t <- s2
    pure $ u /\ apply s1 t

infixr 4 composeSubst as @@

merge :: Subst -> Subst -> Expect Subst
merge s1 s2 = if agree then pure $ s1 <> s2 else mergeFail s1 s2 where
  agree = Array.all
    (\ v -> apply s1 (TVar v) == apply s2 (TVar v))
    (map fst s1 `Array.intersect` map fst s2)

varBind :: Tyvar -> Type -> Expect Subst
varBind u t
  | t == TVar u = pure nullSubst
  | u `Array.elem` tv t = occursCheck u t
  | ku <- kind u
  , kt <- kind t
  , ku /= kt = kindMismatch ku kt
  | otherwise = pure $ u +-> t

mgu :: Type -> Type -> Expect Subst
mgu u t = case u, t of
  TAp l r, TAp l' r' -> do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure $ s2 @@ s1
  TVar n, _ -> varBind n t
  _, TVar n -> varBind n u
  TCon tc1, TCon tc2 | tc1 == tc2 -> pure nullSubst
  _, _ -> typesDoNotUnify u t

match :: Type -> Type -> Expect Subst
match u t = case u, t of
  TAp l r, TAp l' r' -> do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
  TVar n, _ | kind n == kind t -> pure $ n +-> t
  TCon tc1, TCon tc2 | tc1 == tc2 -> pure nullSubst
  _, _ -> typeMismatch u t

-- typeclasses

data Qual t = Then (Array Pred) t

infix 1 Then as :=>

-- NOTE: Another frequently requested extension is to allow classes to accept
-- multiple parameters, which would require a list of Types rather than the
-- single Type in the definition above.
data Pred = IsIn Id Type
derive instance eqPred :: Eq Pred

instance typesPred :: Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn _ t) = tv t

instance typesQual :: Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps `Array.union` tv t

lift :: (Type -> Type -> Maybe Subst) -> Pred -> Pred -> Maybe Subst
lift m (IsIn i t) (IsIn i' t')
  | i == i' = m t t'
  | otherwise = Nothing

mguPred :: Pred -> Pred -> Maybe Subst
mguPred p1 p2 = lift (\ t1 t2 -> hush (mgu t1 t2)) p1 p2

matchPred :: Pred -> Pred -> Maybe Subst
matchPred p1 p2 = lift (\ t1 t2 -> hush (match t1 t2)) p1 p2

-- NOTE: A full Haskell implementation would need to store additional information for each
-- declaration, such as the list of member functions for each class and details of their
-- implementations in each particular instance.
-- We will represent each class by a pair of lists, one containing the name of each
-- superclass, and another containing an entry for each instance declaration:
type Class = Array Id /\ Array Inst
type Inst = Qual Pred

-- The information provided by the class and instance declarations in a given program
-- can be captured by a class environment of type:
type ClassEnv =
  { classes :: Id -> Maybe Class
  , defaults :: Array Type
  }

super :: ClassEnv -> Id -> Array Id
super env i = maybe [] fst $ env.classes i

insts :: ClassEnv -> Id -> Array Inst
insts env i = maybe [] snd $ env.classes i

-- describes how a class environment can be updated to reflect a new binding of
-- a Class value to a given identifier (note: this is just insert for FMaps)
modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce {classes = \ j -> if i == j then Just c else ce.classes j}

initialEnv :: ClassEnv
initialEnv = {classes: \_ -> Nothing, defaults: []}

type EnvTransformer = ClassEnv -> Maybe ClassEnv

-- To add a new class to an environment, we must check that there is not already
-- a class with the same name, and that all of the named superclasses are already
-- defined. This is a simple way of enforcing Haskell’s restriction that the superclass
-- hierarchy be acyclic. Of course, in practice, it will be necessary to topologically
-- sort the set of class declarations in a program to determine a suitable ordering; any
-- cycles in the hierarchy will typically be detected at this stage.
addClass :: Id -> Array Id -> EnvTransformer
addClass i is ce
  | Just _ <- ce.classes i = Nothing
  | Array.any (isNothing <<< ce.classes) is = Nothing
  | otherwise = Just (modify ce i (is /\ []))

-- To add a new instance to a class, we must check that the class to which the instance
-- applies is defined, and that the new instance does not overlap with any previously
-- defined instance
addInst :: Array Pred -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce = z where
  z = case ce.classes i of
    Nothing -> Nothing
    _ -> if Array.any (overlap p) qs then Nothing else Just (modify ce i c)
  its = insts ce i
  c = super ce i /\ ((ps :=> p) : its)
  qs = do
    _ :=> q <- its
    pure q

-- Two instances for a class are said to overlap if there is some predicate that is a
-- substitution instance of the heads of both instance declarations.
overlap :: Pred -> Pred -> Boolean
overlap p q = isJust (mguPred p q)

-- The Haskell report imposes some further restrictions on class and instance dec-
-- larations that are not enforced by the definitions of addClass and addInst. For
-- example, the superclasses of a class should have the same kind as the class itself;
-- the parameters of any predicates in an instance context should be type variables,
-- each of which should appear in the head of the instance; and the type appearing in
-- the head of an instance should consist of a type constructor applied to a sequence
-- of distinct type variable arguments.

bySuper :: ClassEnv -> Pred -> Array Pred
bySuper ce p@(IsIn i t) = p : Array.concat ss where
  ss = do
    i' <- super ce i
    pure $ bySuper ce (IsIn i' t)

-- for a given predicate p = IsIn i t, we can find all the directly relevant instances in a
-- class environment ce by looking in insts ce i . As we have seen, individual instance
-- declarations are mapped into clauses of the form ps :⇒ h. The head predicate h
-- describes the general form of instances that can be constructed from this declara-
-- tion, and we can use matchPred to determine whether this instance is applicable to
-- the given predicate p. If it is applicable, then matching will return a substitution
-- u, and the remaining subgoals are the elements of map (apply u) ps.
byInst :: ClassEnv -> Pred -> Maybe (Array Pred)
byInst ce p@(IsIn i t) = ala First Array.foldMap is where
  is = tryInst <$> insts ce i
    -- it <- insts ce i
    -- pure $ tryInst it
  tryInst (ps :=> h) = do
    u <- matchPred h p
    Just (map (apply u) ps)

-- Given a particular class environment ce, the intention
-- here is that entail ce ps p will be True if, and only if, the predicate p will hold
-- whenever all of the predicates in ps are satisfied:
entail :: ClassEnv -> Array Pred -> Pred -> Boolean
entail ce ps p = Array.any (Array.elem p) (map (bySuper ce) ps) || case byInst ce p of
  Nothing -> false
  Just qs -> Array.all (entail ce ps) qs

inHnf :: Pred -> Boolean
inHnf (IsIn _ t) = hnf t
  where
    hnf (TVar _) = true
    hnf (TCon _) = false
    hnf (TAp t' _) = hnf t'
    hnf _ = false
