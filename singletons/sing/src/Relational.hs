{-# LANGUAGE AllowAmbiguousTypes #-}

module Relational where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

type Attribute = Symbol
type TableName = Symbol

data Query where
    Table :: TableName -> Query
    Union :: Query -> Query -> Query
    Intersect :: Query -> Query -> Query
    Except :: Query -> Query -> Query
    Join :: Query -> Query -> Query
    -- Project :: 



-- data BinOp = Lt | Le | Eq | Ne | Ge | Gt

-- data Selection :: [Attribute] -> Type where
--     SelAV :: Elem a attrs => BinOp -> Int -> Selection attrs
--     SelAA :: Subset '[a,b] attrs => BinOp -> Selection attrs
--     And :: Selection attrs -> Selection attrs -> Selection attrs
--     Or :: Selection attrs -> Selection attrs -> Selection attrs
--     Not :: Selection attrs -> Selection attrs

-- data Algebra :: [Attribute] -> Type where
--     Union :: Algebra attrs -> Algebra attrs -> Algebra attrs
--     Difference :: Algebra attrs -> Algebra attrs -> Algebra attrs
--     Product :: Algebra as -> Algebra bs -> Algebra (Append as bs)
--     Project :: Subset attrs' attrs => Algebra attrs -> Algebra attrs'
--     Selection :: Selection attrs -> Algebra attrs
--     Rename :: forall a b attrs. Algebra attrs -> Algebra (Rename a b attrs)

-- type family Append xs ys where
--     Append '[] ys = ys
--     Append (x ': xs) ys = x ': Append xs ys

-- type family Elem x xs where
--     Elem x (x ': xs) = (() :: Constraint)
--     Elem x (y ': xs) = Elem x xs

-- type family Subset xs ys where
--     Subset '[] ys = (() :: Constraint)
--     Subset (x ': xs) ys = (Elem x ys, Subset xs ys)

-- type family Rename x y zs where
--     Rename _ _ '[] = '[]
--     Rename x y (x ': zs) = y ': Rename x y zs
--     Rename x y (z ': zs) = z ': Rename x y zs


-- rename :: Algebra attrs -> Algebra (Rename "isFriend" "isBusinessContact" attrs)
-- rename = Rename @"isFriend" @"isBusinessContact"

-- select :: Algebra '["isFriend", "isBusinessContact"]
-- select = Selection (Or (SelAV @"isFriend" Eq 1) (SelAV @"isBusinessContact" Eq 1))
