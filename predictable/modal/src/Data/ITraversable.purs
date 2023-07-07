
module Data.ITraversable
  ( class IFoldable
  , ifoldr
  , ifoldMap
  , ifold
  , isequence_
  , itraverse_

  , class ITraversable
  , isequence
  , itraverse
  ) where

import Data.Later (Later)
import Data.List (List)
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Predictable (class Predictable, predict, class Stable, wait)
import Data.Traversable (sequence, foldr)
import Prelude

-- | Infinite folds
class Functor t <= IFoldable t where
  ifoldr :: forall a b. (a -> Later b -> b) -> b -> t a -> b

ifoldMap
  :: forall t m a.
     IFoldable t
  => Monoid m
  => Stable m
  => (a -> m)
  -> t a
  -> m
ifoldMap f = ifoldr (\a b -> f a <> wait b) mempty

ifold
  :: forall t m.
     IFoldable t
  => Monoid m
  => Stable m
  => t m
  -> m
ifold = ifoldr (\a b -> a <> wait b) mempty

isequence_
  :: forall t m a.
     IFoldable t
  => Applicative m
  => Predictable m
  => t (m a)
  -> m Unit
isequence_ = ifoldr (\a b -> a *> void (predict b)) (pure unit)

itraverse_
  :: forall t m a b.
     IFoldable t
  => Applicative m
  => Predictable m
  => (a -> m b)
  -> t a
  -> m Unit
itraverse_ f = ifoldr (\a b -> f a *> void (predict b)) (pure unit)

instance IFoldable Array where ifoldr f = foldr (\a b -> f a (pure b))
instance IFoldable First where ifoldr f = foldr (\a b -> f a (pure b))
instance IFoldable Last where ifoldr f = foldr (\a b -> f a (pure b))
instance IFoldable List where ifoldr f = foldr (\a b -> f a (pure b))
instance IFoldable Maybe where ifoldr f = foldr (\a b -> f a (pure b))
instance IFoldable Lazy.List where ifoldr f = foldr (\a b -> f a (pure b))

