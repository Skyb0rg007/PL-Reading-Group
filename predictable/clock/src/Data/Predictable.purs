
module Data.Predictable
  (class Stable,
   wait,
   class Predictable,
   predict) where

import Prelude
import Data.Later (Later, Delay (Wait))
import Data.Function (applyFlipped)
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Product (Product, product)
import Data.Identity (Identity (Identity))
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Newtype (unwrap)
import Data.Const (Const (Const))
import Control.Monad.Reader.Trans (ReaderT (ReaderT))
import Control.Monad.Writer.Trans (WriterT (WriterT))

class Stable a where
  wait :: forall s. Later s a -> a

instance Stable Unit where
  wait _ = unit

class Functor f <= Predictable f where
  predict :: forall s a. Later s (f a) -> f (Later s a)

instance Predictable (Function r) where
  predict l x = map (applyFlipped x) l

instance (Predictable f, Predictable g) => Predictable (Compose f g) where
  predict l = Compose (map predict (predict (map unwrap l)))

instance (Predictable f, Predictable g) => Predictable (Product f g) where
  predict l = product (predict (map pr1 l)) (predict (map pr2 l))
    where pr1 = unwrap >>> fst
          pr2 = unwrap >>> snd

instance Stable a => Predictable (Tuple a) where
  predict l = Tuple (wait (map fst l)) (map snd l)

instance Predictable Identity where
  predict l = Identity (map unwrap l)

instance Stable a => Predictable (Const a) where
  predict l = Const (wait (map unwrap l))

instance Predictable m => Predictable (ReaderT r m) where
  predict l = ReaderT (map predict (predict (map unwrap l)))

instance (Stable w, Predictable m) => Predictable (WriterT w m) where
  predict l = WriterT (map go (predict (map unwrap l)))
    where
      go x = Tuple (map fst x) (wait (map snd x))


newtype Update p s m a = Update (s -> m (Tuple p a))

class Monoid p <= ApplyAction p s where
  applyAction :: p -> s -> s

instance (Functor m) => Functor (Update p s m) where
  map f (Update u) = Update \s -> map (map f) (u s)

instance (ApplyAction p s, Monad m) => Apply (Update p s m) where
  apply = ap

instance (ApplyAction p s, Monad m) => Applicative (Update p s m) where
  pure a = Update \_ -> pure (Tuple mempty a)

instance (ApplyAction p s, Monad m) => Bind (Update p s m) where
  bind (Update u) f = Update \s -> do
    Tuple p a <- u s
    let Update t = f a
    Tuple p' a' <- t (applyAction p s)
    pure $ Tuple (p <> p') a'

instance (ApplyAction p s, Monad m) => Monad (Update p s m)

-- instance (Stable p, Predictable m) => Predictable (Update p s m) where
--   predict x = Update \s -> predict (map (applyFlipped s <<< unwrap) x)
