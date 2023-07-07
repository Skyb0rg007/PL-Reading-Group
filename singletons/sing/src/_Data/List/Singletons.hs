{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.List.Singletons
    ( SList (..)

    , Append
    , sAppend

    , Map
    , sMap

    , Reverse
    , sReverse

    , Intersperse
    , sIntersperse

    -- * Defunctionalization Symbols
    , AppendSym0
    , AppendSym1
    , IntersperseSym0
    , IntersperseSym1
    , ReverseSym0
    , MapSym0
    , MapSym1
    ) where

import Data.Kind (Type)
import Data.Function.Singletons (Apply, (@@), type (@@), type (~>))
import Data.Bool.Singletons ()
import Data.Singletons (Sing, SingKind (..), SomeSing (..))

-- | List singleton
type SList :: [a] -> Type
data SList a where
    SNil :: SList '[]
    SCons :: Sing x -> Sing xs -> SList (x ': xs)

type instance Sing = SList

instance SingKind a => SingKind [a] where
    type Demote [a] = [Demote a]

    toSing [] = SomeSing SNil
    toSing (x : xs) =
        case (toSing x, toSing xs) of
            (SomeSing x', SomeSing xs') -> SomeSing (SCons x' xs')

    fromSing SNil = []
    fromSing (SCons x xs) = fromSing x : fromSing xs

-- | Type-level list append
type Append :: [a] -> [a] -> [a]
type family Append xs ys where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': (Append xs ys)

sAppend :: Sing xs -> Sing ys -> Sing (Append xs ys)
sAppend SNil ys = ys
sAppend (SCons x xs) ys = SCons x (sAppend xs ys)

-- | Map a function over a type-level list
type Map :: (a ~> b) -> [a] -> [b]
type family Map f xs where
    Map _ '[] = '[]
    Map f (x ': xs) = f @@ x ': Map f xs

sMap :: Sing f -> Sing xs -> Sing (Map f xs)
sMap _ SNil = SNil
sMap f (SCons x xs) = SCons (f @@ x) (sMap f xs)

-- | Reverse a type-level list
type Reverse :: [a] -> [a]
type family Reverse xs where
    Reverse xs = RevAppend xs '[]

sReverse :: Sing xs -> Sing (Reverse xs)
sReverse xs = sRevAppend xs SNil

type RevAppend :: [a] -> [a] -> [a]
type family RevAppend xs ys where
    RevAppend '[] ys = ys
    RevAppend (x ': xs) ys = RevAppend xs (x ': ys)

sRevAppend :: Sing xs -> Sing ys -> Sing (RevAppend xs ys)
sRevAppend SNil ys = ys
sRevAppend (SCons x xs) ys = sRevAppend xs (SCons x ys)

type Intersperse :: a -> [a] -> [a]
type family Intersperse sep xs where
    Intersperse _ '[] = '[]
    Intersperse sep (x ': xs) = x ': PrependToAll sep xs

sIntersperse :: Sing sep -> Sing xs -> Sing (Intersperse sep xs)
sIntersperse _ SNil = SNil
sIntersperse sep (SCons x xs) = SCons x (sPrependToAll sep xs)

type PrependToAll :: a -> [a] -> [a]
type family PrependToAll sep xs where
    PrependToAll _ '[] = '[]
    PrependToAll sep (x ': xs) = sep ': x ': PrependToAll sep xs

sPrependToAll :: Sing sep -> Sing xs -> Sing (PrependToAll sep xs)
sPrependToAll _ SNil = SNil
sPrependToAll sep (SCons x xs) = SCons sep (SCons x (sPrependToAll sep xs))

-- * Defunctionalization symbols

data AppendSym0 :: [a] ~> [a] ~> [a]
data AppendSym1 :: [a] -> [a] ~> [a]

type instance Apply AppendSym0 xs = AppendSym1 xs
type instance Apply (AppendSym1 xs) ys = Append xs ys

data MapSym0 :: (a ~> b) ~> [a] ~> [b]
data MapSym1 :: (a ~> b) -> [a] ~> [b]

type instance Apply MapSym0 f = MapSym1 f
type instance Apply (MapSym1 f) xs = Map f xs

data ReverseSym0 :: [a] ~> [a]

type instance Apply ReverseSym0 xs = Reverse xs

data IntersperseSym0 :: a ~> [a] ~> [a]
data IntersperseSym1 :: a -> [a] ~> [a]

type instance Apply IntersperseSym0 sep = IntersperseSym1 sep
type instance Apply (IntersperseSym1 sep) xs = Intersperse sep xs

