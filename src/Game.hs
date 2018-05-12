module Game
where

import System.IO
import Control.Exception
import System.Directory
import Board
import Data
import AI
import Configuration


-- petla glowna gry
game::State -> IO()
game state = do 
               putStrLn boardStateMsg
               drawBoard state 0 0
               processUserInput state

-- polecenia uzytkownika
processUserInput :: State -> IO ()
processUserInput state = do
                          (operation:rest) <- fmap words getLine
                          case operation of
                            '-':'h':_ -> do 
                                          printHelp
                                          processUserInput state
                            '-':'p':_ -> do 
                                          printInfo [boardStateMsg]
                                          drawBoard state 0 0
                                          processUserInput state
                            '-':'n':_ -> newGame
                            '-':'e':_ -> exitGame
                            '-':'s':_ -> if null rest then do 
                                             printInfo [invalidCommErr]
                                             processUserInput state
                                         else saveGame state rest
                            '-':'l':_ -> if null rest then do 
                                             printInfo [invalidCommErr]
                                             processUserInput state 
                                         else loadGame state rest
                            '-':'m':_ -> if null rest then do 
                                             printInfo [invalidCommErr]
                                             processUserInput state
                                         else do
                                            if (length (head rest) == 1) && isNumber (readMaybe (head rest)::Maybe Int) 
                                            then do parseSheepMovement state rest
                                            else do 
                                                  printInfo [invalidCommErr]
                                                  processUserInput state
                            _ -> do 
                                  printInfo [invalidCommErr]
                                  processUserInput state

-- bezpieczny odczyt stringa
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
              [(val,"")] -> Just val
              _ -> Nothing

-- czy string jest intem
isNumber :: Maybe Int -> Bool
isNumber Nothing = False
isNumber (Just x) = True

-- nowa gra
newGame :: IO ()
newGame = do 
           printInfo [gameStartedMsg]
           printHelp
           game defaultGame

-- wyjscie z gry
exitGame::IO()
exitGame = do 
            printInfo [exitingGameMsg] 
            return()

-- zapis stanu gry 
saveGame::State -> [String] -> IO ()
saveGame state (fileName:_) = do 
                               writeStateToFile state fileName
                               game state

-- zapis planszy do pliku
writeStateToFile::State->String->IO ()
writeStateToFile state fileName = catch (
                                   do
                                    handle <- openFile fileName WriteMode
                                    hPutStrLn handle (toString state)
                                    hClose handle
                                    printInfo [gameSavedMsg]
                                  ) errorHandler
                                  where 
                                    errorHandler :: SomeException -> IO ()
                                    errorHandler e = do printInfo [saveErr]

-- wczytanie stanu gry
loadGame::State -> [String] -> IO ()
loadGame state (fileName:_) = do
                               newState <- loadStateFromFile state fileName
                               game newState

-- wczytanie planszy z pliku
loadStateFromFile::State->String->IO State
loadStateFromFile state fileName = catch (
                                   do
                                    fileExists <- doesFileExist fileName
                                    if fileExists then do
                                       handle <- openFile fileName ReadMode
                                       line <- hGetLine handle
                                       hClose handle 
                                       printInfo [gameLoadedMsg] 
                                       return (fromString (words line))                    
                                    else do
                                       printInfo [fileNameErr]
                                       return state
                                   ) errorHandler
                                   where 
                                    errorHandler :: SomeException -> IO State
                                    errorHandler e = do 
                                                      printInfo [loadErr]
                                                      return state

-- sprawdzenie warunku zwyciestwa obu stron
parseOutcome::(Outcome,State)->IO ()
parseOutcome (outcome, state) = case outcome of
                                  SheepWon -> do printInfo [congratulationMsg, sheepWonMsg]
                                  WolfWon -> do printInfo [congratulationMsg, wolfWonMsg]
                                  _ -> do game state

-- sprawdz ruch owcy Indeks kierunek (SE lub SW)	  
parseSheepMovement::State->[String]->IO ()
parseSheepMovement state (index:direction:_) = do
                                              let sheepId = read index
                                              if elem sheepId [0..3]
                                              then case direction of 
                                                 'L':_ -> trySheepMovement state sheepId (Vector (-1) 1)
                                                 'R':_ -> trySheepMovement state sheepId (Vector 1 1)
                                                 _ -> do
                                                      printInfo [sheepDirErr]
                                                      processUserInput state
                                              else do 
                                                      printInfo [sheepIdxErr]
                                                      processUserInput state
  
-- wykonaj ruch owca
trySheepMovement::State->Int->Vector->IO ()
trySheepMovement state sheepId vector | canSheepMove state sheepId vector = parseOutcome (wolfMoveState (moveSheep state sheepId vector))
                                      | otherwise = do
                                                     printInfo [fieldOccupErr]
                                                     game state