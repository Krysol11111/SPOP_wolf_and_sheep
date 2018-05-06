module Game (initGame)
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
initGame = game (State (Wolf 3 6) [(Sheep 1 0),(Sheep 3 0),(Sheep 5 0),(Sheep 7 0)])

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
                                      "L":_ -> if canSheepMove state sheepId (Vector (-1) 1) 
                                               then do parseOutcome (wolfMoveState (moveSheep state sheepId (Vector (-1) 1)))
                                               else do putStrLn "Zajete pole"; game state
                                      "P":_ -> if canSheepMove state sheepId (Vector 1 1)
                                               then do parseOutcome (wolfMoveState (moveSheep state sheepId (Vector 1 1))) 
                                               else do putStrLn "Zajete pole"; game state
                                      _     -> do putStrLn "Niepoprawny kierunek"
                                                  game state 
                            else do 
                                  putStrLn "Niepoprawna komenda"
                                  game state

parseOutcome::(Outcome,State)->IO ()
parseOutcome (outcome, state) = case outcome of
                                  SheepWon -> do putStrLn "Sheep won"
                                  WolfWon -> do putStrLn "Wolf won"
                                  _ -> do game state

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