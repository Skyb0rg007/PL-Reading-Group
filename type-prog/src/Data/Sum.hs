{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Data.Sum (module Data.Sum) where

import Data.Kind (Constraint, Type)

type Sum :: [Type -> Type] -> Type -> Type
data Sum fs a where
    Here  :: f a -> Sum (f ': fs) a
    There :: Sum fs a -> Sum (f ': fs) a

type Forall :: (k -> Constraint) -> [k] -> Constraint
type family Forall c xs where
    Forall _ '[] = ()
    Forall c (x ': xs) = (c x, Forall c xs)

instance Forall Functor fs => Functor (Sum fs) where
    fmap f (Here x) = Here (fmap f x)
    fmap f (There x) = There (fmap f x)

instance Forall Foldable fs => Foldable (Sum fs) where
    foldMap f (Here x) = foldMap f x
    foldMap f (There x) = foldMap f x

instance (Forall Functor fs, Forall Foldable fs, Forall Traversable fs) => Traversable (Sum fs) where
    traverse f (Here x) = Here <$> traverse f x
    traverse f (There x) = There <$> traverse f x


type Member :: (Type -> Type) -> [Type -> Type] -> Constraint
class Member f fs where
    inject  :: f a -> Sum fs a
    project :: Sum fs a -> Maybe (f a)

instance Member f (f ': fs) where
    inject = Here
    project (Here x) = Just x
    project (There _) = Nothing

instance {-# OVERLAPPABLE #-} Member f fs => Member f (g ': fs) where
    inject = There . inject
    project (Here _) = Nothing
    project (There x) = project x

-- proveForall
--     :: forall c1 c2 x r.
--        (Forall c1 x, forall y. c1 y => c2 y)
--     => (Forall c2 x => r)
--     -> r
-- proveForall k =

-- type Map :: (k1 -> k2) -> [k1] -> [k2]
-- type family Map f xs where
--     Map _ '[] = '[]
--     Map f (x ': xs) = f x ': Map f xs

-- type Foldl :: (k2 -> k1 -> k2) -> k2 -> [k1] -> k2
-- type family Foldl f z xs where
--     Foldl _ z '[] = z
--     Foldl f z (x ': xs) = Foldl f (f z x) xs

