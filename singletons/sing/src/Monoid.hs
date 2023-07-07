{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Monoid where

import Data.Kind (Type)
import Prelude hiding (Monoid)
import Data.Singletons
import Data.List.Singletons
import Data.Singletons.TH
import Data.Foldable.Singletons
import Data.Bool.Singletons
import Data.Eq.Singletons
import Text.Show.Singletons
import Prelude.Singletons

class EqRel k where
    type (=~=) (a :: k) (b :: k) :: Type

    sRefl :: forall (a :: k).
        Sing a
        -> a =~= a

    sSym :: forall (a :: k) (b :: k).
        Sing a
        -> Sing b
        -> a =~= b
        -> b =~= a

    sTrans :: forall (a :: k) b c. Sing a -> Sing b -> Sing c
        -> a =~= b
        -> b =~= c
        -> a =~= c

class Monoid k where
    type Append (a :: k) (b :: k) :: k
    type Empty :: k

    sAppend :: forall (a :: k) (b :: k). Sing a -> Sing b -> Sing (Append a b)

    sEmpty :: Sing (Empty :: k)

    sLeftUnit :: forall (a :: k).
        Sing a
        -> Empty `Append` a =~= a

    sRightUnit :: forall (a :: k).
        Sing a
        -> a `Append` Empty =~= a

    sAssoc :: forall (a :: k) b c.
        Sing a
        -> Sing b
        -> Sing c
        -> (a `Append` (b `Append` c)) =~= ((a `Append` b) `Append` c)


type family ListAppend xs ys where
    ListAppend '[] ys = ys
    ListAppend (x ': xs) ys = x ': ListAppend xs ys

instance EqRel [a] where
    type (=~=) (xs :: [a]) ys = xs :~: ys

    sRefl _ = Refl
    sSym _ _ Refl = Refl
    sTrans _ _ _ Refl Refl = Refl

instance Monoid [a] where
    type Append xs ys = ListAppend xs ys
    type Empty = '[]

    sAppend SNil ys = ys
    sAppend (SCons x xs) ys = SCons x (sAppend xs ys)

    sEmpty = SNil

    sLeftUnit _ = Refl

    sRightUnit SNil = Refl
    sRightUnit (SCons _ xs) =
        case sRightUnit xs of
            Refl -> Refl

    sAssoc SNil _ _ = Refl
    sAssoc (SCons _ xs) ys zs =
        case sAssoc xs ys zs of
            Refl -> Refl

$(singletons [d|
    newtype ListSet a = ListSet [a]

    eqSet :: Eq a => ListSet a -> ListSet a -> Bool
    eqSet (ListSet xs) (ListSet ys) =
        all (`elem` ys) xs && all (`elem` xs) ys
    |])

instance PEq a => EqRel (ListSet a) where
    type (=~=) (x :: ListSet a) (y :: ListSet a) = EqSet x y :~: 'True

    sRefl = undefined
    sTrans = undefined
    sSym = undefined
        
-- instance PEq a => Monoid (ListSet a) where
    
$(singletons [d|

    data Nat = Zero | Succ Nat
        deriving Show

    |])

data LtWitness :: Nat -> Nat -> Type where
    Lt1 :: LtWitness 'Zero ('Succ a)
    Lt2 :: LtWitness a b -> LtWitness ('Succ a) ('Succ b)

class Lt a b where
    witness :: LtWitness a b

instance Lt 'Zero ('Succ a) where
    witness = Lt1

instance Lt a b => Lt ('Succ a) ('Succ b) where
    witness = Lt2 witness

-- nth :: (Lt i n) => ....
-- nth = go witness
--     where
--         go Lt1 ...
--         go Lt2 ...

append :: (SingKind k, Monoid k) => Demote k -> Demote k -> Demote k
append xs ys = 
    case (toSing xs, toSing ys) of
        (SomeSing xs', SomeSing ys') -> fromSing (sAppend xs' ys')

empty :: (SingKind k, Monoid k) => Demote k
empty = fromSing sEmpty







