{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Matrix (
      Array
    , unArray
    , eye
    , identity
    , ones
    , full
    , reshape
    , ravel
    , Data.Matrix.replicate
    ) where

import Control.Applicative (liftA2)
import Data.Kind (Constraint, Type)
import Data.Functor.Const (Const (..))
import Data.Vector qualified as V
import GHC.Exts (proxy#)
import GHC.TypeNats (Nat, KnownNat, natVal', type (*))
import Data.List (unfoldr)

natVal :: forall n. KnownNat n => Int
natVal = fromIntegral $ natVal' (proxy# @n)

type Array :: [Nat] -> Type -> Type
newtype Array shape a = Array { unArray :: V.Vector a }
    deriving newtype (Eq, Ord, Functor, Foldable)

instance Forall KnownNat shape => Applicative (Array shape) where
    pure = full
    liftA2 f (Array a) (Array b) = Array (V.zipWith f a b)

instance (Forall KnownNat shape, Num a) => Num (Array shape a) where
    fromInteger = full . fromInteger
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum

instance (Forall KnownNat shape, Show a) => Show (Array shape a) where
    show (Array v) = unlines . snd $ getConst (cata @KnownNat @shape z alg)
        where
            z :: Const (Bool, [String]) '[]
            z = Const (True, map show (V.toList v))
            alg :: forall y ys. (KnownNat y) => Const (Bool, [String]) ys -> Const (Bool, [String]) (y ': ys)
            alg (Const (b, s)) = Const (False, s')
                where
                    s' = if b
                        then map (brac . unwords) chunks
                        else map unlines chunks
                    chunks = chunksOf (natVal @y) s
                    brac x = "[" ++ x ++ "]"

            chunksOf n = unfoldr \lst ->
                if null lst
                    then Nothing
                    else Just (splitAt n lst)

eye :: forall n m a. (KnownNat n, KnownNat m, Num a) => Array '[n, m] a
eye = Array v
    where
        n = natVal @n
        m = natVal @m
        v = V.generate (n * m) \i -> if mod i m == div i m then 1 else 0

identity :: forall n a. (KnownNat n, Num a) => Array '[n, n] a
identity = eye @n @n

ones :: forall shape a. (Forall KnownNat shape, Num a) => Array shape a
ones = full 1

full :: forall shape a. Forall KnownNat shape => a -> Array shape a
full x = Array (V.replicate n x)
    where
        n = getConst $ cata @KnownNat @shape (Const 1) f
        f :: forall y ys. KnownNat y => Const Int ys -> Const Int (y ': ys)
        f (Const acc) = Const (acc * natVal @y)

reshape :: forall shape' shape a. (Product shape ~ Product shape') => Array shape a -> Array shape' a
reshape (Array v) = Array v

ravel :: forall shape a. Array shape a -> Array '[Product shape] a
ravel = reshape

replicate :: forall n shape a. KnownNat n => Array shape a -> Array (n ': shape) a
replicate (Array v) = Array (V.concat (Prelude.replicate (natVal @n) v))

type Product :: [Nat] -> Nat
type family Product xs where
    Product '[] = 1
    Product (x ': xs) = x * Product xs

-- broadcast :: [Int] -> [Int] -> Maybe [Int]
-- broadcast xs ys = fmap reverse (go (reverse xs) (reverse ys))
--     where
--         go [] [] = Just []
--         go xs [] = go xs [1]
--         go [] ys = go [1] ys
--         go (1 : xs) (y : ys) = (y :) <$> go xs ys
--         go (x : xs) (1 : ys) = (x :) <$> go xs ys
--         go (x : xs) (y : ys) = if x == y then (x :) <$> go xs ys else Nothing

-- test :: Bool
-- test = and
--     [ broadcast [5,4] [1] == Just [5,4]
--     , broadcast [5,4] [4] == Just [5,4]
--     , broadcast [15,3,5] [15,1,5] == Just [15,3,5]
--     , broadcast [15,3,5] [3,5] == Just [15,3,5]
--     , broadcast [15,3,5] [3,1] == Just [15,3,5]
--     , broadcast [3] [4] == Nothing
--     , broadcast [2,1] [8,4,3] == Nothing
--     ]


class (ForallF c xs, Forall Top xs) => Forall c xs where
    cata :: forall r. r '[] -> (forall y ys. (c y, Forall c ys) => r ys -> r (y ': ys)) -> r xs

instance Forall c '[] where
    cata z _ = z

instance (c x, Forall c xs) => Forall c (x ': xs) where
    cata z f = f (cata @c @xs z f)

type ForallF :: (k -> Constraint) -> [k] -> Constraint
type family ForallF c xs where
    ForallF _ '[] = ()
    ForallF c (x ': xs) = (c x, ForallF c xs)

class Top a
instance Top a

