{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Control.Monad.Effect where

import Data.Kind (Constraint, Type)
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO (liftIO))

type Sum :: [Type -> Type] -> Type -> Type
data Sum fs a where
    Here  :: f a -> Sum (f ': fs) a
    There :: Sum fs a -> Sum (f ': fs) a

type Member :: (Type -> Type) -> [Type -> Type] -> Constraint
class Member f fs where
    inject :: f a -> Sum fs a

instance Member f (f ': fs) where
    inject = Here

instance {-# OVERLAPS #-} Member f fs => Member f (g ': fs) where
    inject = There . inject


instance Forall Functor fs => Functor (Sum fs) where
    fmap f (Here x) = Here (fmap f x)
    fmap f (There x) = There (fmap f x)


type Forall :: (k -> Constraint) -> [k] -> Constraint
type family Forall c xs where
    Forall _ '[] = ()
    Forall c (x ': xs) = (c x, Forall c xs)


type Eff :: [Type -> Type] -> Type -> Type
data Eff fs a
    = Pure a
    | Free (Sum fs (Eff fs a))
    deriving Functor

instance Forall Functor fs => Applicative (Eff fs) where
    pure = Pure
    (<*>) = ap

instance Forall Functor fs => Monad (Eff fs) where
    Pure a >>= f = f a
    Free x >>= f = Free (fmap (>>= f) x)

instance (Member IO fs, Forall Functor fs) => MonadIO (Eff fs) where
    liftIO = lift

run :: Eff '[] a -> a
run (Pure a) = a
run (Free x) = case x of

runM :: Monad m => Eff '[m] a -> m a
runM (Pure a) = pure a
runM (Free (Here x)) = x >>= runM
runM (Free (There x)) = case x of

lift :: (Functor f, Member f fs) => f a -> Eff fs a
lift = Free . inject . fmap Pure

handle :: forall f fs a b. (Functor f, Forall Functor fs)
       => (a -> Eff fs b)
       -> (f (Eff fs b) -> Eff fs b)
       -> Eff (f ': fs) a
       -> Eff fs b
handle onPure _ (Pure a) = onPure a
handle onPure onEff (Free (Here x)) = onEff (handle onPure onEff <$> x)
handle onPure onEff (Free (There x)) = Free (handle onPure onEff <$> x)

handleS :: forall f fs s a b. (Functor f, Forall Functor fs)
        => s
        -> (s -> a -> Eff fs b)
        -> (s -> f (s -> Eff fs b) -> Eff fs b)
        -> Eff (f ': fs) a
        -> Eff fs b
handleS s onPure _ (Pure a) = onPure s a
handleS s onPure onEff (Free (Here x)) = onEff s ((\a b -> handleS b onPure onEff a) <$> x)
handleS s onPure onEff (Free (There x)) = Free (handleS s onPure onEff <$> x)

interpret :: forall f fs b. (Forall Functor (f ': fs))
          => (forall a. f a -> Eff fs a)
          -> Eff (f ': fs) b
          -> Eff fs b
interpret _ (Pure a) = pure a
interpret t (Free (Here x)) = t x >>= interpret t
interpret t (Free (There x)) = Free (fmap (interpret t) x)


data Reader r a = Ask (r -> a)
    deriving Functor

ask :: forall r fs. Member (Reader r) fs => Eff fs r
ask = lift (Ask id)

runReader :: forall r fs a. Forall Functor fs => r -> Eff (Reader r ': fs) a -> Eff fs a
runReader r = interpret (\(Ask k) -> pure (k r))


data State s a = Get (s -> a) | Put s a
    deriving Functor

get :: forall s fs. Member (State s) fs => Eff fs s
get = lift (Get id)

put :: forall s fs a. Member (State s) fs => s -> Eff fs ()
put s = lift (Put s ())

-- runState :: forall s fs a. (Forall Functor fs)
--          => Eff (State s ': fs) a
--          -> s
--          -> Eff fs (a, s)
-- runState e s = fmap ($ s) (interpret φ e)
--     where
--         φ :: forall a. State s a -> Eff fs a
--         φ _ = undefined
