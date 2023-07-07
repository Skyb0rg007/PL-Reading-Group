
module Data.Later
  ( Later
  , fix
  , unsafeForce
  , Delay (Now, Wait)
  , unsafeTimeout
  ) where

import Prelude
import Data.Maybe (Maybe (..))

foreign import data Later :: Type -> Type
type role Later representational

foreign import fix :: forall a. (Later a -> a) -> a
foreign import unsafeForce :: forall a. Later a -> a
foreign import lpure :: forall a. a -> Later a
foreign import lmap :: forall a b. (a -> b) -> Later a -> Later b
foreign import lapply :: forall a b. Later (a -> b) -> Later a -> Later b

instance Functor Later where
  map = lmap

instance Apply Later where
  apply = lapply

instance Applicative Later where
  pure = lpure


data Delay a = Now a | Wait (Later (Delay a))

instance Functor Delay where
  map f (Now a) = Now (f a)
  map f (Wait x) = Wait (map (map f) x)

instance Apply Delay where
  apply = ap

instance Applicative Delay where
  pure = Now

instance Bind Delay where
  bind (Now a) f = f a
  bind (Wait x) f = Wait (map (\d -> bind d f) x)

instance Monad Delay

unsafeTimeout :: forall a. Int -> Delay a -> Maybe a
unsafeTimeout _ (Now a) = Just a
unsafeTimeout n (Wait x) =
  if n <= 0
    then Nothing
    else unsafeTimeout (n - 1) (unsafeForce x)
