module Lib
    ( initgame
    ) where
	
import System.Exit
	
data Direction = SW | SE deriving (Enum, Eq)
data Wolf = Wolf Int Int
data Sheep = Sheep Int Int
data State = State Wolf [Sheep]							--TODO state data type

initstate :: State										--TODO initial state
initstate = State (Wolf 2 1) ([])

initgame :: IO ()										
initgame = game initstate

printstate :: State -> String							--TODO create state string
printstate _ = "abc"

getsheepnumber :: String -> Int							--TODO get number of moved sheep
getsheepnumber _ = 1

getsheepdirection :: String -> Direction						--TODO get direction from input
getsheepdirection _ = SE

movesheep :: State -> Int -> Direction -> State				--TODO create new state after sheep movement
movesheep (State (Wolf x y) a) _ _ = State (Wolf x (y-1)) a

movewolf :: State -> State								--TODO minmax + heuristics -> creates state after wolf movement + wolf-losestate
movewolf a = a

parse :: String -> Bool 								--TODO check syntax
parse a = (a == "Kappa")

validate :: String -> Bool								--TODO check semantics
validate command = (getsheepnumber command) == 1 && (getsheepdirection command) == SE

game :: State -> IO()									--TODO main loop (... or recursion?)
game (State (Wolf _ y) _) | y == 0 = do 
	putStrLn "THE END"
	exitWith ExitSuccess
game state = do
	putStrLn (printstate state)
	command <- getLine
	if (parse command) && (validate command)
	 then game (movewolf (movesheep state (getsheepnumber command) (getsheepdirection command)))
	 else putStrLn "Couldn't parse command, try again"
	game (state)