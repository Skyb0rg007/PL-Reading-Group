{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Vec where

import Data.Kind (Type)
import Data.List.Singletons
import Data.Maybe.Singletons
import Data.Singletons
import Data.Singletons.TH
import GHC.TypeLits.Singletons
import Prelude hiding (replicate)
import Prelude.Singletons


$(singletons [d|

    data N = Zero | Succ N
    
    instance Num N where
        fromInteger n =
            if n == 0
                then Zero
                else Succ (fromInteger (n - 1))

        Zero + m = m
        Succ n + m = Succ (n + m)

        Succ n - Succ m = n - m
        Succ n - Zero = Succ n
        Zero - _ = Zero

        Zero * _ = Zero
        Succ n * m = m + n * m

        abs n = n

        signum Zero = Zero
        signum (Succ _) = Succ Zero

    |])

plusZero :: forall a.
       Sing a
    -> (a + 'Zero) :~: a
plusZero SZero = Refl
plusZero (SSucc n) =
    case plusZero n of
        Refl -> Refl

plusSucc :: forall a b.
       Sing a
    -> Sing b
    -> (a + 'Succ b) :~: ('Succ (a + b))
plusSucc SZero _ = Refl
plusSucc (SSucc a) b =
    case plusSucc a b of
        Refl -> Refl

plusAssoc :: forall (a :: N) b c.
       Sing a
    -> Sing b
    -> Sing c
    -> (a + (b + c)) :~: ((a + b) + c)
plusAssoc SZero _ _ = Refl
plusAssoc (SSucc a) b c =
    case plusAssoc a b c of
        Refl -> Refl

plusComm :: forall (a :: N) b.
       Sing a
    -> Sing b
    -> (a + b) :~: (b + a)
plusComm SZero b = case plusZero b of Refl -> Refl
plusComm (SSucc a) b =
    case (plusComm a b, plusSucc b a) of
        (Refl, Refl) -> Refl


