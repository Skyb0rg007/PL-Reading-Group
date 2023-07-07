{-# LANGUAGE UndecidableInstances #-}

module Units.Internal where

import Data.Kind (Constraint, Type)
import Data.Singletons
import GHC.TypeLits (Symbol, Nat)

type Quantity :: dim -> unit -> Type -> Type
data Quantity d u a = Quantity a

type Unit :: unit -> Constraint
class Unit u

type BaseDimension :: Symbol -> unit -> Type
data BaseDimension name unit


type Elem :: k -> [k] -> Constraint
type family Elem x xs where
    Elem x (x ': xs) = ()
    Elem x (y ': xs) = Elem x xs

type Subset :: [k] -> [k] -> Constraint
type family Subset xs ys where
    Subset (x ': xs) ys = (Elem x ys, Subset xs ys)
    Subset '[] _ = ()

type SetEqual :: [k] -> [k] -> Constraint
type family SetEqual xs ys where
    SetEqual xs ys = (Subset xs ys, Subset ys xs)


