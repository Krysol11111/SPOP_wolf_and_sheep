module Lib
    ( initgame
    ) where
	
import System.Exit
	
data Direction = SE | SW deriving (Enum, Eq, Read)

data Vector = Vector Int Int							--something for converting direction to vector move
xVector (Vector x _) = x
yVector (Vector _ y) = y

dirToVector :: Direction -> Vector
dirToVector dir = case dir of 
	SE -> Vector 1 1
	SW -> Vector 1 (-1)
	
moveWolf :: State -> Vector -> State
moveWolf (State (Wolf x y) sheep) (Vector dx dy) = State (Wolf (x+dx) (y+dy)) sheep

--TODO moveSheep :: State -> Int -> Vector -> State


data Wolf = Wolf Int Int
data Sheep = Sheep Int Int
data State = State Wolf [Sheep]							--TODO state data type

initstate :: State										--TODO initial state
initstate = State (Wolf 2 1) ([])

sheepcount :: Int
sheepcount = 4

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
validate command = elem (getsheepnumber command) [1..4] && (getsheepdirection command) == SE

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