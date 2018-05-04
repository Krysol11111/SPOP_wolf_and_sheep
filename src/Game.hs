module Game
where

import System.IO
import System.Directory
import Board
import Data
import AI

{-
Default board setting
-}
initGame::IO ()
initGame = game (State (Wolf 6 3) [(Sheep 0 1),(Sheep 0 3),(Sheep 0 5),(Sheep 0 7)])

{-
Game main loop
-}
game::State->IO()
game state = do
              drawBoard state 0 0
              printOptions
              cmd <- getLine
              executeOption state cmd
  
printOptions::IO ()
printOptions = do 
                putStrLn "r - ruch owcy"
                putStrLn "n - nowa gra"
                putStrLn "k - koniec gry"
                putStrLn "z - zapis stanu gry do pliku"
                putStrLn "w - wczytanie stanu gry z pliku"
                putStrLn "Wybierz operację i zatwierdź Enter"

{-
User options
-}
executeOption::State->[Char]->IO ()
executeOption state option = case option of
    'r':_ -> parseSheepMovement state
    'n':_ -> initGame
    'k':_ -> exitGame
    'z':_ -> writeStateToFile state
    'w':_ -> loadStateFromFile state
    _  -> do putStrLn "Wybrano nieporawna opcje"
             game state
 
exitGame::IO()
exitGame = do
            putStrLn "Koniec gry"
            return()

parseSheepMovement::State->IO()
parseSheepMovement state = do
                            putStrLn "Wybierz index owcy i kierunek (L)ewo lub (P)rawo"
                            (index:direction) <- fmap words getLine
                            let sheepId = read index 
                            if 2 /= (length (index:direction)) 
                            then do 
                                  putStrLn "Niepoprawny index"
                                  game state  
                            else if (elem sheepId [0..3]) 
                            then 
                                case direction of
                                      "L":_ -> game (wolfMove (moveSheep state sheepId (Vector 1 (-1))))
                                      "P":_ -> game (wolfMove (moveSheep state sheepId (Vector 1 1)))
                                      _   -> game state 
                            else do 
                                  putStrLn "Niepoprawna komenda"
                                  game state

parseDirection::[String]->Vector
parseDirection [] = (Vector 0 0) 
parseDirection (x:xs) | x == "L" = (Vector (-1) 1)
                      | x == "R" = (Vector 1 1)
                      | otherwise = parseDirection xs
  
writeStateToFile::State->IO()
writeStateToFile state = do
                          putStrLn "Podaj nazwe pliku:"
                          filePath <- getLine
                          handle <- openFile filePath WriteMode
                          hPutStrLn handle (stateToString state)
                          hClose handle
                          putStrLn "Zapisano stan gry"
                          game state
                          

loadStateFromFile::State->IO()
loadStateFromFile state = do
                     putStrLn "Podaj nazwe pliku:"
                     filePath <- getLine
                     fileExists <- doesFileExist filePath
                     if fileExists then do
                                         handle <- openFile filePath ReadMode
                                         line <- hGetLine handle
                                         putStrLn "Wczytano stan gry"; 
                                         hClose handle            
                                         game (stringToState (words line))                    
                     else do
                           putStrLn "Plik nie istnieje"
                           game state