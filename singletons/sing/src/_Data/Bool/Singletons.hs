{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Bool.Singletons
    ( SBool (..)

    , If
    , sIf
    , IfSym0
    , IfSym1
    , IfSym2

    , Not
    , sNot
    , NotSym0

    , And
    , sAnd
    , AndSym0
    , AndSym1

    , Or
    , sOr
    , OrSym0
    , OrSym1
    ) where

import Data.Function.Singletons (type (~>), Apply)
import Data.Kind (Type)
import Data.Singletons (Sing, SingKind (..), SomeSing (..))

type SBool :: Bool -> Type
data SBool b where
    SFalse :: SBool 'False
    STrue :: SBool 'True

type instance Sing = SBool

instance SingKind Bool where
    type Demote Bool = Bool

    toSing True = SomeSing STrue
    toSing False = SomeSing SFalse

    fromSing STrue = True
    fromSing SFalse = False

-- | Type-level conditional
type If :: Bool -> a -> a -> a
type family If cond x y where
    If 'True x _ = x
    If 'False _ y = y

data IfSym0 :: Bool ~> a ~> a ~> a
data IfSym1 :: Bool -> a ~> a ~> a
data IfSym2 :: Bool -> a -> a ~> a

type instance Apply IfSym0 b = IfSym1 b
type instance Apply (IfSym1 b) x = IfSym2 b x
type instance Apply (IfSym2 b x) y = If b x y

sIf :: Sing cond -> Sing x -> Sing y -> Sing (If cond x y)
sIf STrue x _ = x
sIf SFalse _ y = y

-- | Type-level negation
type Not :: Bool -> Bool
type family Not b = res | res -> b where
    Not 'True = 'False
    Not 'False = 'True

data NotSym0 :: Bool ~> Bool

type instance Apply NotSym0 b = Not b

sNot :: Sing b -> Sing (Not b)
sNot STrue = SFalse
sNot SFalse = STrue

-- | Type-level boolean and
type And :: Bool -> Bool -> Bool
type family And x y where
    And 'True b = b
    And b 'True = b
    And 'False _ = 'False
    And _ 'False = 'False
    And b b = b

data AndSym0 :: Bool ~> Bool ~> Bool
data AndSym1 :: Bool -> Bool ~> Bool

type instance Apply AndSym0 x = AndSym1 x
type instance Apply (AndSym1 x) y = And x y

sAnd :: Sing x -> Sing y -> Sing (And x y)
sAnd STrue b = b
sAnd b STrue = b
sAnd SFalse SFalse = SFalse

-- | Type-level boolean or
type Or :: Bool -> Bool -> Bool
type family Or x y where
    Or 'False b = b
    Or b 'False = b
    Or 'True _ = 'True
    Or _ 'True = 'True
    Or b b = b

data OrSym0 :: Bool ~> Bool ~> Bool
data OrSym1 :: Bool -> Bool ~> Bool

type instance Apply OrSym0 x = OrSym1 x
type instance Apply (OrSym1 x) y = Or x y

sOr :: Sing x -> Sing y -> Sing (Or x y)
sOr SFalse b = b
sOr b SFalse = b
sOr STrue STrue = STrue
