module AI
    (
	testing
    ) where
	
import System.Exit

data Wolf = Wolf Int Int
data Sheep = Sheep Int Int deriving (Eq)
data State = State Wolf [Sheep]
data Vector = Vector Int Int

possibleX = [1..8]
possibleY = [1..8]
possibleWolfMoves = [(Vector 1 1), (Vector 1 (-1)), (Vector (-1) (-1)), (Vector (-1) 1)]
possibleSheepMoves = [(Vector 1 1), (Vector (-1) 1)]

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

	
testState :: State										
testState = State (Wolf 2 1) [(Sheep 1 1), (Sheep 1 2), (Sheep 1 3), (Sheep 1 4)]

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



