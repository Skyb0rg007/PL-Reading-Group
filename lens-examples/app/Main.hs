module Main (main) where

import Control.Lens

main :: IO ()
main = putStrLn "Hello, world!"






data Person = MkPerson
    { _personEmail :: String
    , _personIdent :: Int
    , _personAge   :: Int
    }
    deriving (Show, Eq, Ord)

personEmail :: Lens' Person String
personEmail f (MkPerson e i d) = fmap (\e' -> MkPerson e' i d) (f e)

personIdent :: Lens' Person Int
personIdent f (MkPerson e i d) = fmap (\i' -> MkPerson e i' d) (f i)

personAge :: Lens' Person Int
personAge f (MkPerson e i d) = fmap (\d' -> MkPerson e i d') (f d)

skye :: Person
skye = MkPerson "ssoss@uchicago.edu" 10 24

