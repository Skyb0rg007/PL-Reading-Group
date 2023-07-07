{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Ordinal where

import Data.Kind (Constraint)
import Data.Eq.Singletons
import Data.Ord.Singletons
-- import Data.String.Singletons
-- import Data.Singletons.ShowSing
import Data.Singletons.TH
import Prelude.Singletons
-- import Text.Show.Singletons
import GHC.TypeLits.Singletons
import Data.Singletons.Base.TypeError
-- import GHC.Base.Singletons

$(singletons [d|

    data Tree = Leaf | Node Tree Tree
        deriving Eq


    instance Show Tree where
        showsPrec _ Leaf = showChar '0'
        showsPrec p (Node a b) = go b 1
            where
                coeff n =
                    if n == (1 :: Natural)
                        then id
                        else shows n

                go Leaf n =
                    if a == fromInteger 0
                        then shows n
                    else if a == fromInteger 1
                        then coeff n . showChar 'w'
                    else showParen (p > 10) $
                        coeff n . showString "w^" . showsPrec 11 a
                go (Node c d) n =
                    if a == c
                        then go d (n + 1)
                        else showParen (p > 10) $
                            coeff n
                            . showChar 'w'
                            . (if a == fromInteger 1 then id else showChar '^' . showsPrec 11 a)
                            . showString " + " . showsPrec 9 (Node c d)

    instance Ord Tree where
        _ < Leaf = False
        Leaf < Node _ _ = True
        Node a b < Node c d =
            case compare a c of
                LT -> True
                EQ -> b < d
                GT -> False

        a <= b = a == b || a < b

        a >= b = a == b || b < a

    instance Num Tree where
        fromInteger n =
            if n == 0
                then Leaf
                else Node Leaf (fromInteger (n - 1))

        Leaf + b = b
        a@Node{} + Leaf = a
        Node a c + Node b d =
            if a < b
                then Node b d
                else Node a (c + Node b d)

        Leaf * _ = Leaf
        Node _ _ * Leaf = Leaf
        a@Node{} * Node Leaf d = a + a * d
        Node a c * Node b@Node{} d = Node (a + b) Leaf + Node a c * d

        Leaf - _ = Leaf
        a@Node{} - Leaf = a
        Node a c - Node b d =
            case compare a b of
                LT -> Leaf
                EQ -> c - d
                GT -> Node a c

        signum Leaf = fromInteger 0
        signum Node{} = fromInteger 1

        abs x = x

    first :: Tree -> Tree
    first Leaf = Leaf
    first (Node a _) = a

    isCNF :: Tree -> Bool
    isCNF Leaf = True
    isCNF (Node s t) = isCNF s && isCNF t && first t <= s

    -- omega = ω
    omega :: Tree
    omega = Node (Node Leaf Leaf) Leaf

    -- expt(α) = ω^α
    expt :: Tree -> Tree
    expt t = Node t Leaf
    |])

showOrdinal :: forall (t :: Tree). SingI t => String
showOrdinal = case sShow_ (sing @t) of
    SSym @s -> symbolVal (Proxy :: Proxy s)

type Ordinal :: Tree -> Constraint
type Ordinal t =
    If (IsCNF t)
        (() :: Constraint)
        (TypeError ('Text "The tree " ':$$: 'ShowType t ':$$: 'Text " is not in Cantor Normal Form"))

showCNF :: forall t. (Ordinal t, SingI t) => String
showCNF = showOrdinal @t

plusRightIdentity :: forall (a :: Tree).
                     Sing a
                  -> a + 'Leaf :~: a
plusRightIdentity SLeaf = Refl
plusRightIdentity (SNode _ _) = Refl

geLeftPlus :: forall a b c.
    ( (a >= First b) ~ 'True
    , (a >= First c) ~ 'True
    )
    => Sing a
    -> Sing b
    -> Sing c
    -> (a >= First (b + c)) :~: 'True
geLeftPlus _ SLeaf _ = Refl
geLeftPlus _ (SNode _ _) SLeaf = Refl
geLeftPlus _ (SNode b _) (SNode c _) =
    case b %< c of
        STrue -> Refl
        SFalse -> Refl

cnfSplit :: forall a b. (IsCNF (Node a b) ~ 'True)
    => Sing a
    -> Sing b
    -> (IsCNF a :~: 'True, IsCNF b :~: 'True, (First b <= a) :~: 'True)
cnfSplit SLeaf SLeaf = (Refl, Refl, Refl)

leSplit :: forall a b. (a <= b) ~ 'True => Sing a -> Sing b
    -> Either ((a == b) :~: 'True) ((a < b) :~: 'True)
leSplit _ _ = undefined

plusCNF :: forall a b. (IsCNF a ~ 'True, IsCNF b ~ 'True)
    => Sing a
    -> Sing b
    -> IsCNF (a + b) :~: 'True
plusCNF SLeaf _ = Refl
plusCNF (SNode _ _) SLeaf = Refl
plusCNF (SNode @a' @c a c) (SNode @b' @d b d) = undefined
    -- case a %< b of
    --     STrue -> Refl
    --     SFalse -> case cnfSplit a c of
    --         (Refl, Refl, Refl) -> case cnfSplit a b of
    --             (Refl, Refl, Refl) -> case cnfSplit b d of
    --                 (Refl, Refl, Refl) -> case plusCNF c (SNode b d) of
    --                     (Refl :: IsCNF (c + 'Node b' d) :~: 'True) ->
    --                         case geLeftPlus a c d of
    --                             Refl -> undefined
                    -- case leSplit (sFirst c) a of
                    --     Left Refl -> undefined
                    --     Right Refl -> undefined
        -- Leaf + b = b
        -- a@Node{} + Leaf = a
        -- Node a c + Node b d =
        --     if a < b
        --         then Node b d
        --         else Node a (c + Node b d)

-- plusAssoc :: forall (a :: Tree) (b :: Tree) (c :: Tree).
--              (IsCNF a ~ 'True, IsCNF b ~ 'True, IsCNF c ~ 'True)
--           => Sing a
--           -> Sing b
--           -> Sing c
--           -> a + (b + c) :~: (a + b) + c
-- plusAssoc SLeaf b c = Refl
-- plusAssoc (SNode a a') SLeaf c = Refl
-- plusAssoc (SNode a a') (SNode b b') SLeaf =
--     case plusRightIdentity (SNode a a' %+ SNode b b') of
--         Refl -> Refl
-- plusAssoc (SNode a a') (SNode b b') (SNode c c') =
--     case (a %< b, b %< c) of
--         (STrue, STrue) -> undefined
--         (STrue, SFalse) -> undefined
--         (SFalse, STrue) -> undefined
--         (SFalse, SFalse) ->
--             case a %< c of
--                 STrue -> _

-- ldist :: forall (a :: Tree) b c. (SingI a, SingI b, SingI c)
--       => a * (b + c) :~: a * b + a * c
-- ldist = undefined

-- (w^2)^3
-- (w^2 * w^2) * w^2
--
--
-- (w^2 + 0) * (w^2 + 0)
-- w^(2+2) + ((w^2 + 0) * 0)
