
module Data.Predictable
  ( class Predictable
  , predict
  , class Stable
  , wait
  ) where

import Prelude
import Data.Later (Later, Delay (..))
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Product (Product, product)
import Data.Identity (Identity (Identity))
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Newtype (unwrap)
import Data.Const (Const (Const))
import Control.Monad.Reader.Trans (ReaderT (ReaderT))
import Control.Monad.Writer.Trans (WriterT (WriterT))

-- | A stable value is one which allows for arbitrary delays
-- i.e. it is an unproductive value
class Stable a where
  wait :: Later a -> a

instance Stable Unit where
  wait _ = unit

instance Stable (Delay a) where
  wait = Wait

-- | A predictable functor is one whose structure is always known
-- i.e. it does not contain any sum types
class Functor f <= Predictable f where
  predict :: forall a. Later (f a) -> f (Later a)

instance Predictable (Function r) where
  predict x y = map (\f -> f y) x

instance Predictable Identity where
  predict x = Identity (map unwrap x)

instance Stable a => Predictable (Const a) where
  predict x = Const (wait (map unwrap x))

instance (Predictable f, Predictable g) => Predictable (Compose f g) where
  predict x = Compose (map predict (predict (map unwrap x)))

instance (Predictable f, Predictable g) => Predictable (Product f g) where
  predict x = product (predict (map pr1 x)) (predict (map pr2 x))
    where pr1 = unwrap >>> fst
          pr2 = unwrap >>> snd

instance Stable a => Predictable (Tuple a) where
  predict x = Tuple (wait (map fst x)) (map snd x)

instance Predictable m => Predictable (ReaderT r m) where
  predict x = ReaderT (map predict (predict (map unwrap x)))

instance (Predictable m, Stable w) => Predictable (WriterT w m) where
  predict x = WriterT (map go (predict (map unwrap x)))
    where go y = Tuple (map fst y) (wait (map snd y))

