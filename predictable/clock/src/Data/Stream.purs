
module Data.Stream
  (Stream (Cons, Nil),
   interleave,
   repeat,
   zipWith,
   StreamC (StreamC),
   uncons
  ) where

import Control.Apply (lift2)
import Data.Functor.Flip (Flip (Flip))
import Data.ITraversable (class ITraversable, isequence)
import Data.Later (Later, fix, force)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Predictable (predict, class Stable, wait)
import Data.Tuple (Tuple (Tuple))
import Prelude

data Stream s a
  = Nil
  | Cons a (Later s (Stream s a))

newtype StreamC a = StreamC (forall s. Stream s a)

uncons :: forall a. StreamC a -> Maybe (Tuple a (StreamC a))
uncons (StreamC s) =
  case s of
    Nil -> Nothing
    Cons x xs ->
      case force (map Flip xs) of
        Flip s' -> Just (Tuple x (StreamC s'))

class Functor t <= IFoldable t where
  ifold :: forall m. Monoid m => Stable m => t m -> m

instance IFoldable (Stream s) where
  ifold Nil = mempty
  ifold (Cons x xs) = x <> wait (map ifold xs)

instance Semigroup (Stream s a) where
  append Nil ys = ys
  append (Cons x xs) ys = Cons x (lift2 append xs (pure ys))

instance Monoid (Stream s a) where
  mempty = Nil

instance Functor (Stream s) where
  map _ Nil = Nil
  map f (Cons x xs) = Cons (f x) (map (map f) xs)

instance ITraversable (Stream s) where
  isequence Nil = pure Nil
  isequence (Cons x xs) = lift2 Cons x (predict (map isequence xs))

repeat :: forall s a. a -> Stream s a
repeat x = fix \rec -> Cons x rec

zipWith :: forall s a b c. (a -> b -> c) -> Stream s a -> Stream s b -> Stream s c
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (lift2 (zipWith f) xs ys)

interleave :: forall s a. Stream s a -> Stream s a -> Stream s a
interleave Nil s2 = s2
interleave (Cons x xs) s2 = Cons x (lift2 interleave (pure s2) xs)
