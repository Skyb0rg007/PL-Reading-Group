
module Data.Later
  (Later,
   Clock,
   fix,
   force,
   Delay (Now, Wait)
  ) where

import Prelude
import Control.Apply (lift2)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Functor.Flip (Flip (Flip))
import Data.Newtype (unwrap)
import Data.Either (Either (Left, Right), isLeft, fromLeft', fromRight', either)
import Partial.Unsafe (unsafeCrashWith)
import Data.Tuple (Tuple (Tuple), fst, snd)

data Clock

-- | `Later s a` represents a productive lazy value of type `a`,
-- possibly undefined in scoped context `s`.
foreign import data Later :: Clock -> Type -> Type
type role Later nominal representational

-- | When constructing a value, you may access the result "later"
foreign import fix :: forall κ a. (Later κ a -> a) -> a

-- | Once the value is finished being constructed,
-- you can now access the lazy values, since it's guarenteed to be there
foreign import force :: forall f κ. (forall κ'. Later κ' (f κ')) -> f κ

foreign import lpure :: forall κ a. a -> Later κ a
foreign import lmap :: forall κ a b. (a -> b) -> Later κ a -> Later κ b
foreign import lapply :: forall κ a b. Later κ (a -> b) -> Later κ a -> Later κ b

instance Functor (Later s) where
  map = lmap

instance Apply (Later s) where
  apply = lapply

instance Applicative (Later s) where
  pure = lpure

-- | `Delay s a` represents an unproductive lazy value of type `a`.
-- The value may not exist, but attempts can be made once outside scope `s`.
data Delay κ a = Now a | Wait (Later κ (Delay κ a))

instance Functor (Delay s) where
  map f (Now a) = Now (f a)
  map f (Wait l) = Wait (map (map f) l)

instance Apply (Delay s) where
  apply (Now f) (Now a) = Now (f a)
  apply (Now f) (Wait a) = Wait (map (map f) a)
  apply (Wait f) (Now a) = Wait (map (map (\k -> k a)) f)
  apply (Wait f) (Wait a) = Wait (lift2 apply f a)

instance Applicative (Delay s) where
  pure = Now

instance Bind (Delay s) where
  bind (Now a) f = f a
  bind (Wait l) f = Wait (map (flip bind f) l)

instance Monad (Delay s)

newtype DelayForce f κ = DelayForce (Delay κ (f κ))

distSum :: forall f g. (forall κ. Either (f κ) (g κ)) -> Either (forall κ. f κ) (forall κ. g κ)
distSum (e :: forall κ. Either (f κ) (g κ)) =
  let l :: _ -> Either (forall κ. f κ) (forall κ. g κ)
      l = Left
      r :: _ -> Either (forall κ. f κ) (forall κ. g κ)
      r = Right
   in if isLeft e
        then l (fromLeft' (\_ -> unsafeCrashWith "Impossible!") e)
        else r (fromRight' (\_ -> unsafeCrashWith "Impossible!") e)

distProd :: forall f g. (forall κ. Tuple (f κ) (g κ)) -> Tuple (forall κ. f κ) (forall κ. g κ)
distProd (p :: forall κ. Tuple (f κ) (g κ)) = tup (fst p) (snd p)
  where
    tup :: _ -> _ -> Tuple (forall κ. f κ) (forall κ. g κ)
    tup = Tuple

distLater :: forall f κ'. (forall κ. Later κ' (f κ)) -> Later κ' (forall κ. f κ)
distLater x = x

newtype Timeout f κ = Timeout (Delay κ (f κ))

unTimeout :: forall f κ. Timeout f κ -> Delay κ (f κ)
unTimeout (Timeout x) = x

force' :: forall f. (forall κ. Delay κ (f κ)) -> Either (forall κ. f κ) (forall κ. Delay κ (f κ))
force' d =
  case d of
    Now (a :: forall κ. f κ) -> (Left :: _ -> Either (forall κ. f κ) (forall κ. Delay κ (f κ))) a
    _ -> unsafeCrashWith "asdf"

  -- case d of
  --   Now _ -> (Left :: _ -> Either (forall κ. f κ) (forall κ. Delay κ (f κ))) (fromNow d)
  --   Wait _ -> (Right :: _ -> Either (forall κ. f κ) (forall κ. Delay κ (f κ))) (unTimeout (force (map Timeout (fromWait d))))
  -- where
  --   fromNow :: (forall κ. Delay κ (f κ)) -> (forall κ. f κ)
  --   fromNow (Now a) = a
  --   fromNow (Wait _) = unsafeCrashWith "Impossible!"
  --   fromWait :: (forall κ. Delay κ (f κ)) -> (forall κ. Later κ (Delay κ (f κ)))
  --   fromWait (Now _) = unsafeCrashWith "Impossible!"
  --   fromWait (Wait x) = x

timeout :: forall f. Int -> (forall κ. Delay κ (f κ)) -> Maybe (forall κ. f κ)
timeout n d =
  case force' d of
    Left a -> Just a
    Right (d' :: forall κ. Delay κ (f κ)) ->
      if n <= 0
        then Nothing
        else timeout (n - 1) d'

