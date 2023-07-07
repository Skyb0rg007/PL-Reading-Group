
module Data.Singletons
    ( Sing
    , SomeSing (SomeSing)
    , SingKind (Demote, toSing, fromSing)
    ) where

import Data.Kind (Constraint, Type)

-- | The singleton kind-indexed type family
type Sing :: k -> Type
type family Sing

-- | An existentially-quantified singleton
type SomeSing :: Type -> Type
data SomeSing k = forall (a :: k). SomeSing (Sing a)

-- | `SingKind` classifies the kinds for which singletons are defined
-- Laws:
-- @
-- 'toSing' . 'fromSing' = 'SomeSing'
-- (\\x -> case 'toSing' x of 'SomeSing' r -> 'fromSing' r) = 'id'
-- @
type SingKind :: Type -> Constraint
class SingKind k where
    -- | The base type for a promoted kind
    type Demote k = (r :: Type) | r -> k

    -- | Convert a singleton to its unrefined version
    fromSing :: forall (a :: k). Sing a -> Demote k

    -- | Convert an unrefined type to an existentially quantified singleton
    toSing :: Demote k -> SomeSing k



type SingI :: k -> Constraint
class SingI a where
    sing :: Sing a
