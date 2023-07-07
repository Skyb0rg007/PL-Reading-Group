{-# LANGUAGE RankNTypes #-}

module Control.Monad.Free (
      Free
    , lift
    , fold
    , hoist
    ) where

data Free f a
    = Pure a
    | Free (f (Free f a))

instance Functor f => Functor (Free f) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
    pure = Pure
    (<*>) = (. flip fmap) . (>>=)

instance Functor f => Monad (Free f) where
    Pure a >>= f = f a
    Free x >>= f = Free (fmap (>>= f) x)

lift :: Functor f => f a -> Free f a
lift = Free . fmap pure

fold :: Monad m => (forall a. f a -> m a) -> Free f b -> m b
fold _ (Pure a) = pure a
fold t (Free x) = t x >>= fold t

hoist :: Functor g => (forall a. f a -> g a) -> Free f b -> Free g b
hoist t = fold (lift . t)


