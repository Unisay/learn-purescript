module Functoriality where

import Prelude
import Data.Maybe (Maybe)

newtype ComposeAM a
  = ComposeAM (Array (Maybe a))

newtype ComposeMA a
  = ComposeMA (Maybe (Array a))

instance showComposeAM :: Show a => Show (ComposeAM a) where
  show (ComposeAM a) = "ComposeAM " <> show a

instance composeAMFunctor :: Functor ComposeAM where
  map f (ComposeAM x) = ComposeAM (map (map f) x)

-- ComposeMA functor as a Homework
{-

> :k Compose
(Type -> Type) -> (Type -> Type) -> Type -> Type
> :k Compose Array
(Type -> Type) -> Type -> Type
> :k Compose Array Maybe
Type -> Type
> :k Compose Array Maybe Int
Type
> :k Maybe
Type -> Type
> 

-}
newtype Compose (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = Compose (f (g a))

instance showCompose :: Show (f (g a)) => Show (Compose f g a) where
  show (Compose a) = "Compose " <> show a

instance functorCompose :: (Functor f, Functor g) => Functor (Compose f g) where
  map :: forall a b. (a -> b) -> Compose f g a -> Compose f g b
  map f (Compose x) = Compose $ map (map f) x

type ComposeAM' a
  = Compose Array Maybe a

type ComposeMA' a
  = Compose Maybe Array a

data ComposeP (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeP (f a) (g a)

instance functorComposeP :: (Functor f, Functor g) => Functor (ComposeP f g) where
  map :: forall a b. (a -> b) -> ComposeP f g a -> ComposeP f g b
  map ab (ComposeP fa ga) = ComposeP (map ab fa) (map ab ga)

data ComposeS (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
  = ComposeF (f a)
  | ComposeG (g a)

instance functorComposeS :: (Functor f, Functor g) => Functor (ComposeS f g) where
  map :: forall a b. (a -> b) -> ComposeS f g a -> ComposeS f g b
  map ab = case _ of
    ComposeF fa -> ComposeF $ map ab fa
    ComposeG ga -> ComposeG $ map ab ga
