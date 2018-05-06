module AI
    ( wolfMoveState,moveSheep,canSheepMove,
	testing
    ) where
	
import System.Exit
import Data

possibleX = [0..7]
possibleY = [0..7]
possibleWolfMoves = [(Vector 1 (-1)), (Vector (-1) (-1)), (Vector 1 1), (Vector (-1) 1)]
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
	if (null possibleMoves)
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
didWolfWon (State (Wolf _ y) _) vectors = or( [ y + (yVector vector) == 0 | vector <- vectors] )
	 
	 
	 

wolfMove :: State -> State
wolfMove state =
	moveWolf state (bestWolfMove moves) where
	 moves = [minmaxWolfMove minmaxDepth state vector | vector <- possibleWolfMoves, canWolfMove state vector]

bestWolfMove :: [(Vector, Int)] -> Vector
bestWolfMove list =
	vector where
	 (vector,_) = getBestMove (Vector (-100) (-100),(-1)) list
	 
bestWolfMoveValue :: [(Vector, Int)] -> Int
bestWolfMoveValue list =
	value where
	 (_,value) = getBestMove (Vector (-100) (-100),(-1)) list

getBestMove :: (Vector,Int) -> [(Vector,Int)] -> (Vector,Int)
getBestMove pair [] = pair
getBestMove (best,bestValue) ((contender,contenderValue):rest) = if (bestValue >= contenderValue) then getBestMove (best,bestValue) rest else getBestMove (contender,contenderValue) rest



minmaxWolfMove :: Int -> State -> Vector -> (Vector,Int)
minmaxWolfMove 0 state vector = (vector, (evalBoard (moveWolf state vector)))
minmaxWolfMove i (State (Wolf x y) _) (Vector dx dy) | (y+dy) == 0 = (Vector dx dy,winningValue)
minmaxWolfMove i state vector =
	if (null moves) 
	 then (vector, 0)
	 else (vector, (minimum  moves))
	 where
	 moves = [minmaxSheepMove (i-1) (moveWolf state vector) sheepid sheepVector | sheepid <- sheepids, sheepVector <- possibleSheepMoves, canSheepMove (moveWolf state vector) sheepid sheepVector]
	
minmaxSheepMove :: Int -> State -> Int -> Vector -> Int
minmaxSheepMove i state sheepid vector =
	if (null wolfMoves) then
	 0
	else
	 bestWolfMoveValue wolfMoves
	where
	 newState = moveSheep state sheepid vector
	 wolfMoves = [minmaxWolfMove i newState wolfVector | wolfVector <- possibleWolfMoves, canWolfMove newState wolfVector]
	 
getMinimumScore :: [(Vector, Int)] -> Int -> Int
getMinimumScore [] worstScore = worstScore
getMinimumScore ((_,score):rest) worstScore = if (score < worstScore) then getMinimumScore rest score else getMinimumScore rest worstScore

evalBoard :: State -> Int
evalBoard (State (Wolf x y) sheep) =
	heightScore {-+ sheepDensityScore-} where
	 heightScore = 7-y
	 sheepDensityScore = evalSheepDensity sheep


evalSheepDensity :: [Sheep] -> Int
evalSheepDensity sheep = 
	6-(maxX-minX)+(maxY-minY) where
	 (minX, maxX, minY, maxY) = getExtremeValues sheep (10,  (-1),  10,  (-1))
	 
getExtremeValues :: [Sheep] -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
getExtremeValues [] extremes = extremes
getExtremeValues ((Sheep x y):rest) (minX,maxX,minY,maxY) = 
	getExtremeValues rest (newMinX, newMaxX, newMinY, newMaxY) where
	 newMinX = min x minX
	 newMaxX = max x maxX
	 newMinY = min y minY
	 newMaxY = max y maxY


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



