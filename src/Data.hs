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



stateToString::State->String
stateToString (State (Wolf wx wy) ((Sheep sx sy):xs)) = wolfToString (Wolf wx wy) ++ sheepToString ((Sheep sx sy):xs)

wolfToString::Wolf->String
wolfToString (Wolf x y) = show x ++ " " ++ show y

sheepToString::[Sheep]->String
sheepToString [] = []
sheepToString ((Sheep x y):xs) = " "++ show x ++ " " ++ show y ++ sheepToString (xs)

stringToState::[String]->State
stringToState (x:y:xs) = State (charToWolf x y) (stringToSheep xs)

charToWolf::String->String->Wolf
charToWolf x y = Wolf (read x) (read y)

stringToSheep::[String]->[Sheep]
stringToSheep [] = []
stringToSheep (x:y:xs) = (Sheep (read x) (read y)) : stringToSheep (xs)