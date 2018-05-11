module AI
    ( wolfMoveState,moveSheep,canSheepMove
    ) where
	
import System.Exit
import Data

possibleX = [0..7] -- wymiar x planszy
possibleY = [0..7] -- wymiar y planszy
possibleWolfMoves = [(Vector 1 (-1)), (Vector (-1) (-1)), (Vector 1 1), (Vector (-1) 1)] -- możliwe ruchy wilka
possibleSheepMoves = [(Vector 1 1), (Vector (-1) 1)] -- możliwe ruchy owcy
sheepids = [0..3]
minmaxDepth = 5 --zmniejszane tylko na ruchach wilka
winningValue :: Int
winningValue = 10000

-- poruszenie wilka
moveWolf :: State -> Vector -> State
moveWolf (State (Wolf x y) sheep) (Vector dx dy) = State (Wolf (x+dx) (y+dy)) sheep

-- poruszenie owcy
moveSheep :: State -> Int -> Vector -> State
moveSheep (State wolf sheep) index vector = (State wolf (modifySheep sheep index vector))

modifySheep :: [Sheep] -> Int -> Vector -> [Sheep]
modifySheep ((Sheep x y):rest) 0 (Vector dx dy) = ((Sheep (x+dx) (y+dy)):rest)
modifySheep (sheep:rest) index vector = (sheep:(modifySheep rest (index-1) vector))

-- sprawdzenie możliwości wybraną owcą w podanym kierunku
canSheepMove :: State -> Int -> Vector -> Bool
canSheepMove (State (Wolf wolfX wolfY) sheep) index vector = 
	and ( inBoard : (wolfNotColliding : sheepNotColliding)) where
	 newSheep = (Sheep ((getX (sheep!!index)) + (getX vector)) ((getY (sheep!!index)) + (getY vector)))
	 inBoard = ((elem (getX newSheep) possibleX) && (elem (getY newSheep) possibleY)) --Bool
	 wolfNotColliding = (not((wolfX == (getX newSheep)) && (wolfY ==(getY newSheep)))) --Bool
	 sheepNotColliding = [x /= newSheep | x <- sheep] --[Bool]

-- sprawdzenie możliwości poruszenia wilka w wybranym kierunku
canWolfMove :: State -> Vector -> Bool
canWolfMove (State (Wolf wolfX wolfY) sheep) (Vector dx dy) =
	and ( inBoard : sheepNotColliding) where
	 newWolf = Wolf (wolfX + dx) (wolfY + dy)
	 inBoard = ((elem (getX newWolf) possibleX) && (elem (getY newWolf) possibleY)) --Bool
	 sheepNotColliding = [((getX newWolf) /= (getX x)) || ((getY newWolf) /= (getY x)) | x <- sheep] --[Bool]
	 
-- uruchomienie algorytmu wyboru ruchu wilka
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

-- sprawdzenie warunku zwycięstwa wilka
didWolfWon :: State -> [Vector] -> Bool
didWolfWon (State (Wolf _ y) _) vectors = or( [ y + (getY vector) == 0 | vector <- vectors] )
	 
-- inicjacja ruchu wilka w algorytmie
wolfMove :: State -> State
wolfMove state =
	moveWolf state (bestWolfMove moves) where
	 moves = [minmaxWolfMove minmaxDepth state vector | vector <- possibleWolfMoves, canWolfMove state vector]

-- wybranie aktualnie najlepszego ruchu dla wilka
bestWolfMove :: [(Vector, Int)] -> Vector
bestWolfMove list =
	vector where
	 (vector,_) = getBestMove (Vector (-100) (-100),(-1)) list

-- wybranie najwyższego wyniku z możliwych ruchów
bestWolfMoveValue :: [(Vector, Int)] -> Int
bestWolfMoveValue list =
	value where
	 (_,value) = getBestMove (Vector (-100) (-100),(-1)) list

-- wybranie najlepszego ruchu z wartością
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

-- heurystyka oceniająca wartość danego układu planszy dla wilka
evalBoard :: State -> Int
evalBoard (State (Wolf x y) sheep) =
	heightScore + wolfFreedomScore {-+ sheepDensityScore-} where
	 heightScore = (7-y) * 4
	 sheepDensityScore = evalSheepDensity sheep
	 wolfFreedomScore = length [move | move <- possibleWolfMoves, canWolfMove (State (Wolf x y) sheep) move]

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

