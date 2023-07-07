{-# LANGUAGE UndecidableInstances #-}

module Sing
    ( Sing
    , SomeSing (..)
    , SingKind (..)
    , Decision (..)
    , SDecide (..)
    , Apply
    , type (@@)

    , SBool (..)
    , SList (..)

    , If
    , sIf
    , Not
    , sNot
    , And
    , sAnd
    , Or
    , sOr
    , Bool_
    , sBool_

    , SSemigroup (..)
    , SMonoid (..)
    ) where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (:~:) (Refl))
import GHC.TypeLits (TypeError, ErrorMessage (Text, ShowType, (:<>:)))
import Data.Void (Void)
import Unsafe.Coerce (unsafeCoerce)

-- | Singleton type family
type Sing :: k -> Type
type family Sing

-- | Existentially quantified singleton
type SomeSing :: Type -> Type
data SomeSing k = forall (a :: k). SomeSing (Sing a)

-- | Typeclass that allows for conversion between value and type level
type SingKind :: Type -> Constraint
class SingKind k where
    type Demote k = (r :: Type) | r -> k

    fromSing :: forall (a :: k). Sing a -> Demote k
    toSing :: Demote k -> SomeSing k

data Decision a
    = Proved a
    | Disproved (a -> Void)

type SDecide :: Type -> Constraint
class SDecide k where
    (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)

------------------------------------------------------------------------------

data TyFun :: Type -> Type -> Type

type (~>) :: Type -> Type -> Type
type (~>) a b = TyFun a b -> Type
infixr 0 ~>

type Apply :: (k1 ~> k2) -> k1 -> k2
type family Apply f x

type (@@) f x = Apply f x
infixl 9 @@

type SLambda :: (k1 ~> k2) -> Type
newtype SLambda f = SLambda (forall t. Sing t -> Sing (Apply f t))

(@@) :: forall k1 k2 (f :: k1 ~> k2) (x :: k1). Sing f -> Sing x -> Sing (Apply f x)
SLambda f @@ x = f x

type instance Sing = SLambda

instance (SingKind k1, SingKind k2) => SingKind (k1 ~> k2) where
    type Demote (k1 ~> k2) = Demote k1 -> Demote k2

    fromSing (SLambda f) x =
        case toSing x of
            SomeSing x' -> fromSing (f x')

    toSing f = SomeSing $ SLambda \x ->
        case toSing (f (fromSing x)) of
            SomeSing r -> unsafeCoerce r

------------------------------------------------------------------------------

data SBool :: Bool -> Type where
    STrue :: SBool 'True
    SFalse :: SBool 'False

instance Show (SBool 'True) where
    show STrue = "STrue"

instance Show (SBool 'False) where
    show SFalse = "SFalse"

type instance Sing = SBool

instance SingKind Bool where
    type Demote Bool = Bool

    fromSing STrue = True
    fromSing SFalse = False

    toSing True = SomeSing STrue
    toSing False = SomeSing SFalse

instance SDecide Bool where
    STrue %~ STrue = Proved Refl
    STrue %~ SFalse = Disproved \case
    SFalse %~ STrue = Disproved \case
    SFalse %~ SFalse = Proved Refl


data SList :: forall a. [a] -> Type where
    SNil :: SList '[]
    SCons :: Sing x -> Sing xs -> SList (x ': xs)

instance (forall (a :: k) sing. Sing ~ sing => Show (sing a)) => Show (SList (z :: [k])) where
    showsPrec _ SNil = showString "SNil"
    showsPrec d (SCons x xs) =
        showParen (d > 10) $
            showString "SCons " . showsPrec 11 x . showChar ' ' . showsPrec 11 xs

type instance Sing = SList

instance SingKind k => SingKind [k] where
    type Demote [k] = [Demote k]

    fromSing SNil = []
    fromSing (SCons x xs) = fromSing x : fromSing xs

    toSing [] = SomeSing SNil
    toSing (x : xs) =
        case (toSing x, toSing xs) of
            (SomeSing x', SomeSing xs') -> SomeSing (SCons x' xs')

instance SDecide a => SDecide [a] where
    SNil %~ SNil = Proved Refl
    SNil %~ SCons _ _ = Disproved \case
    SCons _ _ %~ SNil = Disproved \case
    SCons x xs %~ SCons y ys =
        case (x %~ y, xs %~ ys) of
            (Disproved k, _) -> Disproved \case { Refl -> k Refl }
            (_, Disproved k) -> Disproved \case { Refl -> k Refl }
            (Proved Refl, Proved Refl) -> Proved Refl

-----------------------------------------------------------------------------

type Map :: (k1 ~> k2) -> [k1] -> [k2]
type family Map f xs where
    Map _ '[] = '[]
    Map f (x ': xs) = Apply f x ': Map f xs

data Map_Sym0 :: (k1 ~> k2) ~> [k1] ~> [k2]
data Map_Sym1 (f :: k1 ~> k2) :: [k1] ~> [k2]

type instance Apply Map_Sym0 f = Map_Sym1 f
type instance Apply (Map_Sym1 f) x = Map f x

type Foldl :: (b ~> a ~> b) -> b -> [a] -> b
type family Foldl f z xs where
    Foldl _ z '[] = z
    Foldl f z (x ': xs) = Foldl f (Apply (Apply f z) x) xs

type If :: Bool -> k -> k -> k
type family If cond x y where
    If 'True x _ = x
    If 'False _ y = y

sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue x _ = x
sIf SFalse _ y = y

type Not :: Bool -> Bool
type family Not b where
    Not 'True = 'False
    Not 'False = 'True

data Not_Sym0 :: Bool ~> Bool
type instance Apply Not_Sym0 x = Not x

sNot :: Sing a -> Sing (Not a)
sNot STrue = SFalse
sNot SFalse = STrue

type And :: Bool -> Bool -> Bool
type family And b1 b2 where
    And 'False _ = 'False
    And _ 'False = 'False
    And 'True b = b
    And b 'True = b
    And b b = b

sAnd :: Sing a -> Sing b -> Sing (And a b)
sAnd SFalse _ = SFalse
sAnd _ SFalse = SFalse
sAnd STrue STrue = STrue

type Or :: Bool -> Bool -> Bool
type family Or b1 b2 where
    Or 'True _ = 'True
    Or _ 'True = 'True
    Or 'False b = b
    Or b 'False = b
    Or b b = b

sOr :: Sing a -> Sing b -> Sing (Or a b)
sOr STrue _ = STrue
sOr _ STrue = STrue
sOr SFalse SFalse = SFalse

type Bool_ :: a -> a -> Bool -> a
type family Bool_ x y b where
    Bool_ x _ 'True = x
    Bool_ _ y 'False = y

data Bool_Sym0 :: a ~> a ~> Bool ~> a
data Bool_Sym1 (x :: a) :: a ~> Bool ~> a
data Bool_Sym2 (x :: a) (y :: a) :: Bool ~> a

type instance Apply Bool_Sym0 x = Bool_Sym1 x
type instance Apply (Bool_Sym1 x) y = Bool_Sym2 x y
type instance Apply (Bool_Sym2 x y) b = Bool_ x y b

sBool_ :: forall (k :: Type) (x :: k) (y :: k) (b :: Bool).
    Sing x ->
    Sing y ->
    Sing b ->
    Sing (Bool_ x y b)
sBool_ x _ STrue = x
sBool_ _ y SFalse = y

class SSemigroup k where
    type Append (a :: k) (b :: k) :: k

    sAppend
        :: Sing (a :: k)
        -> Sing (b :: k)
        -> Sing (Append a b)

    sAppendAssoc
        :: Sing (a :: k)
        -> Sing (b :: k)
        -> Sing (c :: k)
        -> (Append (Append a b) c) :~: (Append a (Append b c))

class SSemigroup k => SMonoid k where
    type Empty :: k

    sEmpty :: Proxy k -> Sing (Empty :: k)

    sAppendEmptyL
        :: Sing (a :: k)
        -> (Append Empty a) :~: a

    sAppendEmptyR
        :: Sing (a :: k)
        -> (Append a Empty) :~: a

type family ListAppend xs ys where
    ListAppend '[] ys = ys
    ListAppend (x ': xs) ys = x ': ListAppend xs ys

instance SSemigroup [a] where
    type Append xs ys = ListAppend xs ys

    sAppend SNil ys = ys
    sAppend (SCons x xs) ys = SCons x (sAppend xs ys)

    sAppendAssoc SNil _ _ = Refl
    sAppendAssoc (SCons _ xs) ys zs =
        case sAppendAssoc xs ys zs of
            Refl -> Refl

instance SMonoid [a] where
    type Empty = '[]

    sEmpty Proxy = SNil

    sAppendEmptyL SNil = Refl
    sAppendEmptyL (SCons _ _) = Refl

    sAppendEmptyR SNil = Refl
    sAppendEmptyR (SCons _ xs) = case sAppendEmptyR xs of Refl -> Refl

data Append_Sym0 :: a ~> a ~> a
data Append_Sym1 x :: a ~> a

type instance Apply Append_Sym0 x = Append_Sym1 x
type instance Apply (Append_Sym1 x) y = Append x y


type Booleans :: [Bool]
type Booleans = '[ 'True, 'True, 'False, 'True]

type EnsureTrue :: Bool -> Constraint
type family EnsureTrue b where
    EnsureTrue 'True = ()
    EnsureTrue b = TypeError ( 'Text "Expected 'True, got " ':<>: 'ShowType b )

data EnsureTrue_Sym0 :: Bool ~> Constraint

type instance Apply EnsureTrue_Sym0 b = EnsureTrue b

type FoldMapConstraints :: (a ~> Constraint) -> Constraint -> a -> Constraint
type family FoldMapConstraints f cs x where
    FoldMapConstraints f cs x = (Apply f x, cs)

data FoldMapConstraints_Sym0 :: (a ~> Constraint) ~> Constraint ~> a ~> Constraint
data FoldMapConstraints_Sym1 (f :: a ~> Constraint) :: Constraint ~> a ~> Constraint
data FoldMapConstraints_Sym2 (f :: a ~> Constraint) (c :: Constraint) :: a ~> Constraint

type instance Apply FoldMapConstraints_Sym0 f = FoldMapConstraints_Sym1 f
type instance Apply (FoldMapConstraints_Sym1 f) x = FoldMapConstraints_Sym2 f x
type instance Apply (FoldMapConstraints_Sym2 f x) cs = FoldMapConstraints f x cs

type Negated :: [Bool]
type Negated = Map Not_Sym0 Booleans

foo :: Foldl (FoldMapConstraints_Sym1 EnsureTrue_Sym0) (() :: Constraint) xs => Proxy xs
foo = Proxy

bar :: Proxy '[ 'True, 'True ]
bar = foo

-- baz :: Proxy '[ 'True, 'False, 'True ]
-- baz = foo

