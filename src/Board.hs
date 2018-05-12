module Board
where

import Data

-- domyslny symbol wilka - W
wolfSymbol = "W"

-- domyslny symbol pustego czarnego pola - #
emptySymbol = "#"

-- rysowanie planszy
drawBoard::State->Int->Int->IO()
drawBoard state i j | j < 8 = do 
                               putStrLn ("|"++(drawLine state i j)++"|")
                               drawBoard state 0 (j+1)
                    | otherwise = putStrLn ""

-- rysowanie wiersza
drawLine::State->Int->Int->String
drawLine state i j | i < 8 = (drawCell state i j) ++ (drawLine state (i+1) j)
                   | otherwise = "" 

-- rysowanie komorki planszy
drawCell::State->Int->Int->String
drawCell state i j | (j `mod` 2 == 0) && (i `mod` 2 == 0) = "_"
                   | (j `mod` 2 /= 0) && (i `mod` 2 /= 0) = "_"
                   | otherwise = drawBoardPiece state i j

-- rysowanie pionka
drawBoardPiece::State->Int->Int->String
drawBoardPiece (State (Wolf wx wy) sheeps) nx ny | nx == wx && ny == wy = wolfSymbol
                                                 | otherwise = drawSheep sheeps nx ny 0

-- rysowanie owcy o zadanych wspolrzednych (domyslny znak - indeks na liscie owiec)
drawSheep::[Sheep]->Int->Int->Int->String
drawSheep [] _ _ _ = emptySymbol
drawSheep ((Sheep x y):xs) nx ny startIdx| nx == x && ny == y = show startIdx
                                         | otherwise = drawSheep xs nx ny (startIdx+1)