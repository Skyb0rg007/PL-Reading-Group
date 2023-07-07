{-# LANGUAGE UndecidableInstances #-}

module Data.Function.Singletons
    ( type (~>)
    , Apply
    , type (@@)
    , TyFun

    , SLambda
    , (@@)
    ) where

import Data.Singletons (Sing, SingKind (..), SomeSing (..))
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

-- | Kind of a type-level function
type TyFun :: Type -> Type -> Type
data TyFun a b

-- | Defunctionalized type function
type (~>) :: Type -> Type -> Type
type (~>) a b = TyFun a b -> Type
infixr 0 ~>

-- | Type-level function application
type Apply :: (k1 ~> k2) -> k1 -> k2
type family Apply f x

-- | Alias for 'Apply'
type (@@) :: (k1 ~> k2) -> k1 -> k2
type (@@) f x = Apply f x

-- | Defunctionalized lambda
type SLambda :: (k1 ~> k2) -> Type
newtype SLambda f = SLambda (forall t. Sing t -> Sing (Apply f t))

-- | Application of a defunctionalized lambda
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

