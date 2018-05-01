module AI
    ( wolfMove,
	testing
    ) where
	
import System.Exit

data Wolf = Wolf Int Int
data Sheep = Sheep Int Int deriving (Eq)
data State = State Wolf [Sheep]
data Vector = Vector Int Int
data Outcome = SheepWon | WolfWon | Undetermined

possibleX = [0..7]
possibleY = [0..7]
possibleWolfMoves = [(Vector 1 1), (Vector 1 (-1)), (Vector (-1) (-1)), (Vector (-1) 1)]
possibleSheepMoves = [(Vector 1 1), (Vector (-1) 1)]
sheepids = [0..3]
minmaxDepth = 5 --zmniejszane tylko na ruchach wilka
winningValue :: Int
winningValue = 10000

xSheep (Sheep x y) = x
ySheep (Sheep x y) = y

xWolf (Wolf x y) = x
yWolf (Wolf x y) = y
	
xVector (Vector x _) = x
yVector (Vector _ y) = y
	
moveWolf :: State -> Vector -> State
moveWolf (State (Wolf x y) sheep) (Vector dx dy) = State (Wolf (x+dx) (y+dy)) sheep

moveSheep :: State -> Int -> Vector -> State
moveSheep (State wolf sheep) index vector = (State wolf (modifySheep sheep index vector))

modifySheep :: [Sheep] -> Int -> Vector -> [Sheep]
modifySheep ((Sheep x y):rest) 0 (Vector dx dy) = ((Sheep (x+dx) (y+dy)):rest)
modifySheep (sheep:rest) index vector = (sheep:(modifySheep rest (index-1) vector))

canSheepMove :: State -> Int -> Vector -> Bool
canSheepMove (State (Wolf wolfX wolfY) sheep) index vector = 
	and ( inBoard : (wolfNotColliding : sheepNotColliding)) where
	 newSheep = (Sheep ((xSheep (sheep!!index)) + (xVector vector)) ((ySheep (sheep!!index)) + (yVector vector)))
	 inBoard = ((elem (xSheep newSheep) possibleX) && (elem (ySheep newSheep) possibleY)) --Bool
	 wolfNotColliding = (not((wolfX == (xSheep newSheep)) && (wolfY ==(ySheep newSheep)))) --Bool
	 sheepNotColliding = [x /= newSheep | x <- sheep] --[Bool]
	 
canWolfMove :: State -> Vector -> Bool
canWolfMove (State (Wolf wolfX wolfY) sheep) (Vector dx dy) =
	and ( inBoard : sheepNotColliding) where
	 newWolf = Wolf (wolfX + dx) (wolfY + dy)
	 inBoard = ((elem (xWolf newWolf) possibleX) && (elem (yWolf newWolf) possibleY)) --Bool
	 sheepNotColliding = [((xWolf newWolf) /= (xSheep x)) || ((yWolf newWolf) /= (ySheep x)) | x <- sheep] --[Bool]
	 
--TODO operacje na Wolf i Sheep wspólne


wolfMoveState :: State -> (Outcome,State)
wolfMoveState state = 
	if (possibleMoves == [])
	then
	 (SheepWon,state)
	else
	 if (didWolfWon state possibleMoves)
	 then
	  (WolfWon,state)
	 else
	  (Undetermined,wolfMove state)
	where
	 possibleMoves = [vector | vector <- possibleWolfMoves, canWolfMove state vector]
	 
didWolfWon :: State -> [Vector] -> Bool
didWolfWon (State (Wolf _ y) _) vectors = or( [ y + (yVector vector) | vector <- vectors] )
	 
	 
	 

wolfMove :: State -> State
wolfMove state =
	moveWolf state (bestWolfMove moves) where
	 moves = [minmaxWolfMove minmaxDepth state vector | vector <- possibleWolfMoves, canWolfMove state vector]

bestWolfMove :: [(Vector, Int)] -> Vector
bestWolfMove list =
	vector where
	 (vector,_) = getBestMove (Vector (-100) (-100),(-1)) list

getBestMove :: (Vector,Int) -> [(Vector,Int)] -> (Vector,Int)
getBestMove pair [] = pair
getBestMove (best,bestValue) ((contender,contenderValue):rest) = if (bestValue >= contenderValue) then getBestMove (best,bestValue) rest else getBestMove (contender,contenderValue) rest



minmaxWolfMove :: Int -> State -> Vector -> (Vector,Int)
minmaxWolfMove 0 state vector = (vector, (heuristics (moveWolf state vector)))
minmaxWolfMove i (State (Wolf x y) _) (Vector dx dy) | (y+dy) == 0 = (Vector dx dy,winningValue)
minmaxWolfMove i state vector =
	(vector, (maximum moves)) where
	 moves = [minmaxSheepMove (i-1) (moveWolf state vector) sheepid sheepVector | sheepid <- sheepids, sheepVector <- possibleSheepMoves, canSheepMove (moveWolf state vector) sheepid sheepVector]
	
minmaxSheepMove :: Int -> State -> Int -> Vector -> Int
minmaxSheepMove i state sheepid vector =
	if (wolfMoves == []) then
	 0
	else
	 getMinimumScore wolfMoves winningValue 
	where
	 newState = moveSheep state sheepid vector
	 wolfMoves = [minmaxWolfMove i newState wolfVector | wolfVector <- possibleWolfMoves, canWolfMove newState wolfVector]
	 
getMinimumScore :: [(Vector, Int)] -> Int -> Int
getMinimumScore [] worstScore = worstScore
getMinimumScore ((_,score):rest) worstScore = if (score < worstScore) then getMinimumScore rest score else getMinimumScore rest worstScore


testState :: State										
testState = State (Wolf 1 0) [(Sheep 0 0), (Sheep 0 1), (Sheep 0 2), (Sheep 0 3)]

testing :: IO()
testing = do
 if (canSheepMove testState 0 (Vector 1 0)) then putStrLn "Owca 0 nie powinna móc się ruszyć o 1 0\n" else putStrLn "Success\n"
 if (canSheepMove testState 1 (Vector 1 (-1))) then putStrLn "Owca 1 nie powinna móc się ruszyć o 1 -1\n" else putStrLn "Success\n"
 if (canSheepMove testState 0 (Vector 0 1)) then putStrLn "Owca 0 nie powinna móc się ruszyć o 0 1\n" else putStrLn "Success\n"
 if (canSheepMove testState 0 (Vector 1 1)) then putStrLn "Success\n" else putStrLn "Owca 0 powinna móc się ruszyć o 1 1\n"
 if (canSheepMove testState 0 (Vector (-1) 1)) then putStrLn "Owca 0 nie powinna móc się ruszyć o -1 1\n" else putStrLn "Success\n"
 if (canWolfMove testState (Vector 1 (-1))) then putStrLn "Wilk nie powinien móc się ruszyć o 1 -1\n" else putStrLn "Success\n"
 if (canWolfMove testState (Vector (-1) 1)) then putStrLn "Wilk nie powinien móc się ruszyć o -1 1\n" else putStrLn "Success\n"
 if (canWolfMove testState (Vector 1 1)) then putStrLn "Success\n" else putStrLn "Wilk powinien móc się ruszyć o 1 1\n"



