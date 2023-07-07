{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Ord.Singletons
    ( SOrd (..)
    , SOrdering (..)
    , type (<)
    , type (>)
    , type (>=)
    , Compare
    ) where

import Data.Kind (Type)
import Data.Singletons (Sing, SingKind (..), SomeSing (..))
-- import Data.Singletons.Function (Apply, type (~>))
import Data.Bool.Singletons (SBool (..), And, Or, Not, If)
import Data.Eq.Singletons (SEq (..))
import Data.Type.Equality (type (:~:) (..))

-- * Lifted Ord class

class SEq a => SOrd a where
    type (<=) (x :: a) (y :: a) :: Bool

    (%<=) :: Sing (x :: a) -> Sing y -> Sing (x <= y)

    -- | Comparability (∀xy. x ≤ y ∨ y ≤ x)
    sLeComparable :: Sing (x :: a) -> Sing y -> Or (x <= y) (y <= x) :~: 'True
    -- | Transitivity (∀xyz. x ≤ y ∧ y ≤ z ⇒ x ≤ z)
    sLeTrans :: Sing (x :: a) -> Sing y -> Sing z -> And (x <= y) (y <= z) :~: 'True -> (x <= z) :~: 'True
    -- | Reflexivity (∀x. x ≤ x)
    sLeRefl :: Sing (x :: a) -> (x <= x) :~: 'True
    -- | Anti-symmetry (∀xy. x ≤ y ∧ y ≤ x ⇒ x == y)
    sLeAntisym :: Sing (x :: a) -> Sing y -> And (x <= y) (y <= x) :~: 'True -> (x == y) :~: 'True

type (<) :: a -> a -> Bool
type (<) x y = Not (y <= x)

type (>) :: a -> a -> Bool
type (>) x y = Not (x <= y)

type (>=) :: a -> a -> Bool
type (>=) x y = y <= x

type Compare :: a -> a -> Ordering
type Compare x y = If (x == y) 'EQ (If (x <= y) 'LT 'GT)

-- * Ordering type

type SOrdering :: Ordering -> Type
data SOrdering a where
    SLT :: SOrdering 'LT
    SEQ :: SOrdering 'EQ
    SGT :: SOrdering 'GT

type instance Sing = SOrdering

instance SingKind Ordering where
    type Demote Ordering = Ordering

    toSing LT = SomeSing SLT
    toSing EQ = SomeSing SEQ
    toSing GT = SomeSing SGT

    fromSing SLT = LT
    fromSing SEQ = EQ
    fromSing SGT = LT

type family EqOrdering x y where
    EqOrdering x x = 'True
    EqOrdering 'LT 'EQ = 'False
    EqOrdering 'LT 'GT = 'False
    EqOrdering 'EQ 'LT = 'False
    EqOrdering 'EQ 'GT = 'False
    EqOrdering 'GT 'LT = 'False
    EqOrdering 'GT 'EQ = 'False

instance SEq Ordering where
    type (==) x y = EqOrdering x y

    SLT %== SLT = STrue
    SLT %== SEQ = SFalse
    SLT %== SGT = SFalse
    SEQ %== SLT = SFalse
    SEQ %== SEQ = STrue
    SEQ %== SGT = SFalse
    SGT %== SLT = SFalse
    SGT %== SEQ = SFalse
    SGT %== SGT = STrue

    sEqRefl SLT = Refl
    sEqRefl SEQ = Refl
    sEqRefl SGT = Refl

    sEqSym SLT SLT = Refl
    sEqSym SLT SEQ = Refl
    sEqSym SLT SGT = Refl
    sEqSym SEQ SLT = Refl
    sEqSym SEQ SEQ = Refl
    sEqSym SEQ SGT = Refl
    sEqSym SGT SLT = Refl
    sEqSym SGT SEQ = Refl
    sEqSym SGT SGT = Refl

    sEqTrans SLT SLT SLT Refl = Refl
    sEqTrans SEQ SEQ SEQ Refl = Refl
    sEqTrans SGT SGT SGT Refl = Refl
    sEqTrans SLT SEQ _ r = case r of
    sEqTrans SLT SGT _ r = case r of
    sEqTrans SEQ SLT _ r = case r of
    sEqTrans SEQ SGT _ r = case r of
    sEqTrans SGT SLT _ r = case r of
    sEqTrans SGT SEQ _ r = case r of
    sEqTrans _ SLT SEQ r = case r of
    sEqTrans _ SGT SEQ r = case r of
    sEqTrans _ SLT SGT r = case r of
    sEqTrans _ SGT SLT r = case r of
    sEqTrans _ SEQ SLT r = case r of
    sEqTrans _ SEQ SGT r = case r of

type family LeOrdering x y where
    LeOrdering x x = 'True
    LeOrdering 'LT 'EQ = 'True
    LeOrdering 'LT 'GT = 'True
    LeOrdering 'EQ 'LT = 'False
    LeOrdering 'EQ 'GT = 'True
    LeOrdering 'GT 'LT = 'False
    LeOrdering 'GT 'EQ = 'False

instance SOrd Ordering where
    type (<=) x y = LeOrdering x y

    SEQ %<= SEQ = STrue
    SLT %<= SLT = STrue
    SGT %<= SGT = STrue
    SLT %<= SEQ = STrue
    SLT %<= SGT = STrue
    SEQ %<= SLT = SFalse
    SEQ %<= SGT = STrue
    SGT %<= SLT = SFalse
    SGT %<= SEQ = SFalse

    sLeRefl SLT = Refl
    sLeRefl SEQ = Refl
    sLeRefl SGT = Refl

    sLeComparable SLT SLT = Refl
    sLeComparable SLT SEQ = Refl
    sLeComparable SLT SGT = Refl
    sLeComparable SEQ SLT = Refl
    sLeComparable SEQ SEQ = Refl
    sLeComparable SEQ SGT = Refl
    sLeComparable SGT SLT = Refl
    sLeComparable SGT SEQ = Refl
    sLeComparable SGT SGT = Refl

    sLeTrans SLT SLT _ Refl = Refl
    sLeTrans SLT SEQ SEQ Refl = Refl
    sLeTrans SLT SEQ SGT Refl = Refl
    sLeTrans SLT SGT SGT Refl = Refl
    sLeTrans SEQ SEQ SEQ Refl = Refl
    sLeTrans SEQ SEQ SGT Refl = Refl
    sLeTrans SEQ SGT SGT Refl = Refl
    sLeTrans SGT SGT SGT Refl = Refl
    sLeTrans SEQ SLT _ r = case r of
    sLeTrans SGT SLT _ r = case r of
    sLeTrans SGT SEQ _ r = case r of
    sLeTrans _ SEQ SLT r = case r of
    sLeTrans _ SGT SLT r = case r of
    sLeTrans _ SGT SEQ r = case r of

    sLeAntisym SLT SLT Refl = Refl
    sLeAntisym SEQ SEQ Refl = Refl
    sLeAntisym SGT SGT Refl = Refl
    sLeAntisym SLT SEQ r = case r of
    sLeAntisym SLT SGT r = case r of
    sLeAntisym SEQ SLT r = case r of
    sLeAntisym SEQ SGT r = case r of
    sLeAntisym SGT SLT r = case r of
    sLeAntisym SGT SEQ r = case r of
