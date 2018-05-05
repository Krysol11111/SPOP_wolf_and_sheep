module Board
where

import Data

drawBoard::State->Int->Int->IO()
drawBoard state i j | j < 8 = do 
                               putStrLn ((drawLine state i j))
                               drawBoard state 0 (j+1)
                    | otherwise = putStrLn ""

drawLine::State->Int->Int->String
drawLine state i j | i < 8 = (drawCell state i j) ++ (drawLine state (i+1) j)
                   | otherwise = "" 

drawCell::State->Int->Int->String
drawCell state i j | (j `mod` 2 == 0) && (i `mod` 2 == 0) = "_"
                   | (j `mod` 2 /= 0) && (i `mod` 2 /= 0) = "_"
                   | otherwise = drawWolf state i j

drawWolf::State->Int->Int->String
drawWolf (State (Wolf x y) xs) nx ny | nx == x && ny == y = "W"
                                     | otherwise = drawSheep xs nx ny 0
 
drawSheep::[Sheep]->Int->Int->Int->String
drawSheep [] _ _ _ = "#"
drawSheep ((Sheep x y):xs) nx ny index| nx == x && ny == y = show index
                                      | otherwise = drawSheep xs nx ny (index+1)