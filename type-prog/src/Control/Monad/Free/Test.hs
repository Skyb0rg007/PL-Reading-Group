{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}

module Control.Monad.Free.Test where

import Control.Exception         (throwIO, ErrorCall (ErrorCall))
import Control.Monad.Free        (Free, lift, fold, hoist)
import Control.Monad.Trans.State qualified as S
import Control.Monad.Trans.Except qualified as E
import Data.Functor.Identity     (Identity (Identity, runIdentity))
import Data.IORef                (newIORef, writeIORef, readIORef)
import System.Random             (randomIO)






data State a
    = Get (Int -> a)
    | Put Int a
    deriving Functor

get :: Free State Int
get = lift (Get id)

put :: Int -> Free State ()
put n = lift (Put n ())

newtype StateMonad a = StateMonad { runStateMonad :: Int -> (a, Int) }
    deriving (Functor, Applicative, Monad) via (S.State Int)

runState :: Free State a -> Int -> (a, Int)
runState = runStateMonad . fold \case
    Put n k -> StateMonad \_ -> (k, n)
    Get k   -> StateMonad \n -> (k n, n)

runStateIO :: Free State a -> Int -> IO (a, Int)
runStateIO x n = do
    r <- newIORef n
    let φ :: forall a. State a -> IO a
        φ (Put m k) = writeIORef r m >> pure k
        φ (Get k)   = readIORef r >>= pure . k
    res <- fold φ x
    n' <- readIORef r
    pure (res, n')








data Except a
    = Throw String
    deriving Functor

throw :: String -> Free Except a
throw msg = lift (Throw msg)

runExcept :: Free Except a -> Either String a
runExcept = fold \case
    Throw e -> Left e

runExceptIO :: Free Except a -> IO a
runExceptIO = fold \case
    Throw e -> throwIO (ErrorCall e)


data NonDet a
    = Choose (Bool -> a)
    deriving Functor

choose :: Free NonDet Bool
choose = lift (Choose id)

(<|>) :: Free NonDet a -> Free NonDet a -> Free NonDet a
x <|> y = choose >>= \b -> if b then x else y

runNonDetList :: Free NonDet a -> [a]
runNonDetList = fold \case
    Choose k -> [k True, k False]

runNonDetFirst :: Free NonDet a -> a
runNonDetFirst = runIdentity . fold \case
    Choose k -> Identity (k True)

runNonDetRand :: Free NonDet a -> IO a
runNonDetRand = fold \case
    Choose k -> k <$> randomIO




data StateExcept a = State (State a) | Except (Except a)
    deriving Functor

liftState :: Free State a -> Free StateExcept a
liftState = hoist State

liftExcept :: Free Except a -> Free StateExcept a
liftExcept = hoist Except

prog :: Free StateExcept ()
prog = do
    n <- liftState get
    if n < 0
        then liftExcept (throw "negative")
        else liftState (put (n + 1))
    


-- Manual composition of `Either String` and `State Int`
newtype StateExceptMonad a = StateExceptM { runStateExceptM :: Int -> Either String (a, Int) }
    deriving (Functor, Applicative, Monad) via (S.StateT Int (Either String))

runStateExcept :: Free StateExcept a -> Int -> Either String (a, Int)
runStateExcept = runStateExceptM . fold \case
    State (Get k)    -> StateExceptM \n -> Right (k n, n)
    State (Put n k)  -> StateExceptM \_ -> Right (k, n)
    Except (Throw e) -> StateExceptM \_ -> Left e


-- Manual composition of `State Int` and `Either String`
newtype StateExceptMonad2 a = StateExceptM2 { runStateExceptM2 :: Int -> (Either String a, Int) }
    deriving (Functor, Applicative, Monad) via (E.ExceptT String (S.State Int))

runStateExcept2 :: Free StateExcept a -> Int -> (Either String a, Int)
runStateExcept2 = runStateExceptM2 . fold \case
    State (Get k)    -> StateExceptM2 \n -> (Right (k n), n)
    State (Put n k)  -> StateExceptM2 \_ -> (Right k, n)
    Except (Throw e) -> StateExceptM2 \n -> (Left e, n)


