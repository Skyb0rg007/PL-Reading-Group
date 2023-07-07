
module Data.Eq.Singletons
    ( SEq (..)
    , type (/=)
    , (%/=)
    ) where

import Data.Singletons (Sing)
import Data.Bool.Singletons (Not, sNot, SBool (..), And)
import Data.Type.Equality (type (:~:) (..))

class SEq a where
    -- | Type family representing a decidable equality relation at type level
    -- If you want judgemental equality, use (:~:) from Data.Type.Equality
    type (==) (x :: a) (y :: a) :: Bool

    -- | Value-level computation for the type family
    (%==) :: Sing (x :: a) -> Sing y -> Sing (x == y)

    -- | Reflexivity (∀x. x == x)
    sEqRefl :: Sing (x :: a) -> (x == x) :~: 'True

    -- | Symmetry (∀xy. x == y ⇒ y == x)
    sEqSym :: Sing (x :: a) -> Sing y -> (x == y) :~: (y == x)

    -- | Transitivity (∀xyz. x == y ∧ y == z ⇒ x == z)
    sEqTrans :: Sing (x :: a) -> Sing y -> Sing z -> And (x == y) (y == z) :~: 'True -> (x == z) :~: 'True

type x /= y = Not (x == y)

(%/=) :: SEq a => Sing (x :: a) -> Sing y -> Sing (x /= y)
x %/= y = sNot (x %== y)

type family EqBool x y where
    EqBool b b = 'True
    EqBool 'False 'True = 'False
    EqBool 'True 'False = 'False

instance SEq Bool where
    type (==) x y = EqBool x y

    STrue %== STrue = STrue
    STrue %== SFalse = SFalse
    SFalse %== STrue = SFalse
    SFalse %== SFalse = STrue

    sEqRefl STrue = Refl
    sEqRefl SFalse = Refl

    sEqSym STrue STrue = Refl
    sEqSym STrue SFalse = Refl
    sEqSym SFalse STrue = Refl
    sEqSym SFalse SFalse = Refl

    sEqTrans STrue STrue STrue Refl = Refl
    sEqTrans STrue SFalse STrue r = case r of
    sEqTrans SFalse STrue STrue r = case r of
    sEqTrans SFalse SFalse STrue r = case r of
    sEqTrans STrue STrue SFalse r = case r of
    sEqTrans STrue SFalse SFalse r = case r of
    sEqTrans SFalse STrue SFalse r = case r of
    sEqTrans SFalse SFalse SFalse Refl = Refl
