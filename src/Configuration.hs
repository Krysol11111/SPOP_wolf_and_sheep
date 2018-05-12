module Configuration
where
import Data

-- domyslna konfiguracja planszy
defaultGame::State
defaultGame = (State (Wolf 3 6) [(Sheep 1 0),(Sheep 3 0),(Sheep 5 0),(Sheep 7 0)])

-- Dialogi w grze    
gameStartedMsg     = "|          NEW GAME STARTED         |"
boardStateMsg      = "|         CURRENT BOARD STATE:      |"
congratulationMsg  = "|          CONGRATULATIONS          |"
wolfWonMsg         = "|             WOLF WON!             |"
sheepWonMsg        = "|            SHEEPS WON!            |"
gameSavedMsg       = "|            GAME SAVED             |"
gameLoadedMsg      = "|            GAME LOADED            |"
exitingGameMsg     = "|            EXITING GAME           |"
invalidMoveErr     = "|            INVALID MOVE           |"
helpCmdMsg         = "| print help    : -h                |"
saveCmdMsg         = "| save game     : -s [fileName]     |"
loadCmdMsg         = "| load game     : -l [fileName]     |"
newGameCmdMsg      = "| start new game: -n                |"
exitCmdMsg         = "| exit game     : -e                |"
moveCmdMsg         = "| move sheep    : -m [0-3] [L/R]    |"
printCmdMsg        = "| print board   : -p                |"
possOperationMsg   = "| Possible commands:                |"
enterMsg           = "| Write command and hit 'Enter':    |"
invalidCommErr     = "|          INVALID COMMAND          |"
fileNameErr        = "|           FILE NOT FOUND          |"
sheepIdxErr        = "|         INVALID SHEEP INDEX       |"
sheepDirErr        = "|       INVALID SHEEP DIRECTION     |"
fieldOccupErr      = "| FIELD IS OCCUPIED OR OUT OF RANGE |"
saveErr            = "|         FILE SAVING ERROR         |"
loadErr            = "|         FILE LOADING ERROR        |"

printHelp:: IO ()
printHelp = printInfo [possOperationMsg, helpCmdMsg, printCmdMsg, newGameCmdMsg, saveCmdMsg, loadCmdMsg, exitCmdMsg, moveCmdMsg, enterMsg]

printInfo::[String]->IO()
printInfo msgs = do 
                        putStrLn "====================================="
                        mapM (putStrLn) msgs
                        putStrLn "====================================="