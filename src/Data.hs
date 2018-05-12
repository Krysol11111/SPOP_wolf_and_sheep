module Data
where

import Data.Char

data Wolf = Wolf Int Int deriving Show
data Sheep = Sheep Int Int deriving (Eq, Show)
data State = State Wolf [Sheep] deriving Show
data Vector = Vector Int Int 
data Outcome = SheepWon | WolfWon | Undetermined

class CanGetXY a where
 getX :: a -> Int
 getY :: a -> Int
 
instance CanGetXY Sheep where
 getX (Sheep x y) = x
 getY (Sheep x y) = y
 
instance CanGetXY Wolf where
 getX (Wolf x y) = x
 getY (Wolf x y) = y
 
instance CanGetXY Vector where
 getX (Vector x y) = x
 getY (Vector x y) = y

class StringConversion a where
 fromString :: [String] -> a
 toString :: a -> String

instance StringConversion State where
 fromString (x:y:xs) = (State (fromString [x,y]) (fromStringList xs))
 toString (State wolf sheeps) = toString wolf ++ concat (toStringList sheeps)

instance StringConversion Wolf where
 fromString (x:y:_) = Wolf (read x) (read y)
 toString (Wolf wx wy) = show wx ++ " " ++ show wy

class StringListConversion a where
 fromStringList :: [String] -> [a]
 toStringList :: [a] -> [String]
 
instance StringListConversion Sheep where
 fromStringList [] = []
 fromStringList (x:y:xs) = (Sheep (read x) (read y)) : fromStringList (xs)
 toStringList [] = []
 toStringList ((Sheep x y):xs) = (" " ++ show x ++ " " ++ show y) : toStringList xs