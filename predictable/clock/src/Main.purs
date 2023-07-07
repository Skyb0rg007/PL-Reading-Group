module Main where

import Prelude

import Data.List as List
import Effect (Effect)
import Effect.Console (log)
import Data.Later (Later, fix, force, Delay (Now, Wait))
import Data.ITraversable (class ITraversable)
import Data.Predictable (class Predictable, predict)
import Data.Tuple (Tuple (Tuple))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
-- import Data.Function (applyFlipped)

main :: Effect Unit
main = do
  log "Hello, world!"

  log $ show $ take 10 nats


collatz :: forall κ. BigInt -> Delay κ Int
collatz m = fix go m 0 where
  go rec n acc
    | n <= BigInt.fromInt 1 = Now acc
    | BigInt.even n = Wait (go' (BigInt.shr n 1.0) (acc + 1))
    where go' x y = rec <*> pure x <*> pure y
    | otherwise     = Wait (go' (BigInt.fromInt 3 * n + BigInt.fromInt 1) (acc + 1))
    where go' x y = rec <*> pure x <*> pure y

data Stream s a
  = Nil
  | Cons a (Later s (Stream s a))

instance showStream :: Show a => Show (Stream s a) where
  show Nil = "Nil"
  show (Cons x _) = "(Cons " <> show x <> " " <> "(Later _))"

instance functorStream :: Functor (Stream s) where
  map _ Nil = Nil
  map f (Cons x xs) = Cons (f x) (map (map f) xs)

instance itraversableStream :: ITraversable (Stream s) where
  isequence = fix $ \rec s ->
    case s of
      Nil -> pure Nil
      Cons x xs -> Cons <$> x <*> predict (rec <*> xs)

ibackquence :: forall f s a. Applicative f => Predictable f => Stream s (f a) -> f (Stream s a)
ibackquence = fix $ \rec s ->
  case s of
      Nil -> pure Nil
      Cons x xs -> flip Cons <$> predict (rec <*> xs) <*> x

zip :: forall s a b. Stream s a -> Stream s b -> Stream s (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons x xs) (Cons y ys) =
  Cons (Tuple x y) (zip <$> xs <*> ys)

newtype FlipStream a s = FlipStream (Stream s a)

forceStream :: forall s a. (forall t. Later t (Stream t a)) -> Stream s a
forceStream s =
  case force (map FlipStream s) of
    FlipStream s' -> s'

--------------------------------------


newtype Update s a = Update (s -> (Tuple (s -> s) a))

instance functorUpdate :: Functor (Update s) where
  map f (Update k) = Update (\s -> map f (k s))

instance applyUpdate :: Apply (Update s) where
  apply = ap

instance applicativeUpdate :: Applicative (Update s) where
  pure a = Update (\_ -> Tuple (\x -> x) a)

instance bindUpdate :: Bind (Update s) where
  bind (Update u) f =
    Update $ \s ->
      let (Tuple p a) = u s
          Update t = f a
          (Tuple p' a') = t (p s)
       in (Tuple (compose p p') a')

instance monadUpdate :: Monad (Update s)

-- instance predictableUpdate :: Predictable (Update s) where
--   predict x = Update $ \s -> predict (map (compose (applyFlipped s) runUpdate) x)
--     where runUpdate (Update k) = k

--------------------------------------


ones :: forall s. Stream s Int
ones = fix (\self -> Cons 1 self)

nats :: forall s. Stream s Int
nats = fix (\self -> Cons 1 (map (map (\x -> x + 1)) self))

take :: forall a. Int -> (forall s. Stream s a) -> List.List a
take n (Cons x xs) =
  if n > 0
    then List.Cons x (take (n - 1) (forceStream xs))
    else List.Nil
take _ Nil = List.Nil

-- map _ Nil = Nil
-- map f (Cons x Nil) = Cons (f x) Nil
-- map f (Cons x (Cons y xs)) = Cons (f x) (Cons (f y) (map f xs))



