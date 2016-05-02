module Data.Breadcrumbs where

import Prelude

import Data.List (List(..))
import Data.List (filter) as L

infixr 6 Cons as :

data Breadcrumbs a = Breadcrumb (List a) a (List a)

instance functorBreadcrumbs :: Functor Breadcrumbs where
  map f (Breadcrumb up x down) = Breadcrumb (map f up) (f x) (map f down)

moveUp :: forall a. Breadcrumbs a -> Breadcrumbs a
moveUp (Breadcrumb (u : us) curr ds) = Breadcrumb us u (curr : ds)
moveUp x = x

moveDown :: forall a. Breadcrumbs a -> Breadcrumbs a
moveDown (Breadcrumb us curr (d : ds)) = Breadcrumb (curr : us) d ds
moveDown x = x

mkBreadcrumb :: forall a. a -> Breadcrumbs a
mkBreadcrumb x = Breadcrumb Nil x Nil

consUp :: forall a. a -> Breadcrumbs a -> Breadcrumbs a
consUp x (Breadcrumb us c ds) = Breadcrumb (x : us) c ds

consDown :: forall a. a -> Breadcrumbs a -> Breadcrumbs a
consDown x (Breadcrumb us c ds) = Breadcrumb us c (x : ds)

goToBottom :: forall a. Breadcrumbs a -> Breadcrumbs a
goToBottom (Breadcrumb us c Nil) = Breadcrumb us c Nil
goToBottom bs = goToBottom $ moveDown bs

goToTop :: forall a. Breadcrumbs a -> Breadcrumbs a
goToTop (Breadcrumb Nil c ds) = Breadcrumb Nil c ds
goToTop bs = goToTop $ moveUp bs

insert :: forall a. a -> Breadcrumbs a -> Breadcrumbs a
insert x bs = case goToBottom bs of
                   Breadcrumb us c _ -> Breadcrumb (c : us) x Nil

extract :: forall a. Breadcrumbs a -> a
extract (Breadcrumb _ c _) = c

filter :: forall a. (a -> Boolean) -> Breadcrumbs a -> Breadcrumbs a
filter f (Breadcrumb us c ds) = Breadcrumb (L.filter f us) c (L.filter f ds)
