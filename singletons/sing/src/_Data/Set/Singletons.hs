{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Set.Singletons
    ( Set
    , empty
    ) where

import Data.Kind (Type)
import Data.Singletons
import Data.Bool.Singletons
import Data.Eq.Singletons
import Data.Ord.Singletons
import Data.Type.Equality

data Set a where
    Empty :: Set a
    NonEmpty :: NESet a min -> Set a

data NESet a min where
    Singleton :: Sing min -> NESet a min
    Cons :: Sing (x :: a) -> (x < min) :~: 'True -> NESet a min -> NESet a x

empty :: Set a
empty = Empty

singleton :: Sing a -> Set a
singleton x = NonEmpty (Singleton x)

mySet :: Set Ordering
mySet = NonEmpty $ Cons SLT Refl (Singleton SEQ)

