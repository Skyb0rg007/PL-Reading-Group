
module Data.ITraversable (class ITraversable, isequence, itraverse) where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)
import Data.Maybe.Last (Last)
import Data.Predictable (class Predictable)
import Data.Traversable (sequence)
import Data.Monoid.Endo (Endo (..))
import Data.Newtype (unwrap)
import Prelude

class Functor t <= ITraversable t where
  isequence :: forall m a. Applicative m => Predictable m => t (m a) -> m (t a)

itraverse
  :: forall t m a b. ITraversable t
  => Applicative m
  => Predictable m
  => (a -> m b)
  -> t a
  -> m (t b)
itraverse f = isequence <<< map f

instance ITraversable Array where isequence = sequence
instance ITraversable First where isequence = sequence
instance ITraversable Last where isequence = sequence
instance ITraversable List where isequence = sequence
instance ITraversable Maybe where isequence = sequence


