{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Main (main) where

import Data.Sum (Member, Forall)
import Control.Monad.Effect
import Control.Monad (when)

main :: IO ()
main = do
    print $ run $ handleWriter @String (handleState @Int prog 0)


counter :: (Member (State Int) fs, Forall Functor fs) => Eff fs Int
counter = do
    n <- get
    put (n + 1)
    pure n

prog :: (Member (State Int) fs, Member (Writer String) fs, Forall Functor fs)
     => Eff fs ()
prog = do
    n <- counter
    tell ("Got " ++ show n)
    when (n < 10)
        prog

data State s a
    = Get (s -> a)
    | Put s a
    deriving (Functor)

get :: (Member (State s) fs) => Eff fs s
get = send (Get id)

put :: (Member (State s) fs) => s -> Eff fs ()
put s = send (Put s ())

handleState :: forall s fs a. (Forall Functor fs) => Eff (State s ': fs) a -> s -> Eff fs (s, a)
handleState = handleS
    (\s a -> pure (s, a))
    (\s e ->
        case e of
            Get k -> k s s
            Put s' k -> k s')

data Writer w a
    = Tell w a
    deriving (Functor)

tell :: (Member (Writer w) fs) => w -> Eff fs ()
tell w = send (Tell w ())

handleWriter :: forall w fs a. (Forall Functor fs) => Eff (Writer w ': fs) a -> Eff fs ([w], a)
handleWriter e = handleS
    (\w a -> pure (reverse w, a))
    (\w (Tell x k) -> k (x : w))
    e
    []

