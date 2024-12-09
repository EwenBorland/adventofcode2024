module Day06 where

import Tools.Parsing
import Tools.Tuples
import qualified Data.IntMap.Strict as IntMap
import Data.List

fNorth :: Int
fEast :: Int
fSouth :: Int
fWest :: Int
fNorth = 0
fEast = 1
fSouth = 2
fWest = 3

obstacle :: Char
obstacle = '#'

charAtPos :: (Int, Int) -> [String] -> Char
charAtPos (r,c) lab = (lab !! r) !! c

outOfBounds :: (Int,Int) -> [String] -> Bool 
outOfBounds (r,c) lab = (r < 0) || (r > labMaxRow) || (c < 0) || (c > labMaxCol) 
    where
        labMaxRow = length lab -1
        labMaxCol = length (head lab) -1

getPosFront :: (Int,Int) -> Int -> (Int, Int)
getPosFront (r,c) dir
    | dir == fNorth = (r - 1, c)
    | dir == fEast = (r, c + 1)
    | dir == fSouth = (r + 1, c)
    | dir == fWest = (r, c - 1)
    | otherwise = (0,0)

-- step pos dir lab = (newPos, newDir, stillInBounds)
step :: (Int, Int) -> Int -> [String] -> Int -> ((Int, Int), Int, Bool)
step pos dir lab recursionDepth
    | recursionDepth > 5 = ((0,0),-1,False) -- were going in circles
    | frontOOB = ((0,0),0,False) -- new position would be outside the lab area
    | charAtPos posFront lab == obstacle = step pos dirSide lab (recursionDepth + 1)
    | otherwise = (posFront, dir, True)
    where
        posFront = getPosFront pos dir
        frontOOB = outOfBounds posFront lab
        dirSide = if dir + 1 > fWest then fNorth else dir + 1
         

-- walk pos dir lab = stepCount
walk :: (Int, Int) -> Int -> [String] -> [((Int,Int),Int)]
walk pos dir lab = 
    if not stillInBounds
    then [(pos,dir)]
    else (pos,dir) : walk newPos newDir lab
    where
        stepInfo = step pos dir lab 0
        newPos = Tools.Tuples.elem3Fst stepInfo
        newDir = Tools.Tuples.elem3Snd stepInfo
        stillInBounds = Tools.Tuples.elem3Trd stepInfo

countUniqueSteps :: [((Int,Int),Int)] -> Int
countUniqueSteps stepList = count
    where 
        rows = IntMap.fromListWith (++) [(fst (fst s), [snd (fst s)]) | s <- stepList]  
        count = sum [length (nub row)| row <- IntMap.elems rows]

mergeRows :: [[(Int,Int)]] -> [(Int,Int)]
mergeRows [] = []
mergeRows listOfRows = head listOfRows ++ mergeRows (tail listOfRows)


removeDuplicateSteps :: [((Int,Int),Int)] -> [(Int,Int)]
removeDuplicateSteps stepList = newRows
    where 
        rows = IntMap.fromListWith (++) [(fst (fst s), [snd (fst s)]) | s <- stepList] 
        uniqueByRow = [(rowIndex, nub (rows IntMap.! rowIndex))| rowIndex <- IntMap.keys rows]
        newRowsByRow = [[(fst r, colIndex)| colIndex <- snd r]|r <- uniqueByRow]
        newRows = mergeRows newRowsByRow

canCreateNewLab :: Int -> Int -> [String] -> Bool
canCreateNewLab row col lab = not (outOfBounds (row,col) lab || c == obstacle || c == '^')
    where c = charAtPos (row,col) lab

-- returns a new lab with an obstruction at the input row/col
createNewLab :: Int -> Int -> [String] -> [String]
createNewLab row col lab = newLab
    where
        newLab = [if ir == row then [if ic == col then obstacle else (lab !! ir) !! ic  | ic <- [0.. length (lab !! ir) -1]] else lab !! ir| ir <- [0.. length lab -1]]

walkWithLoopChecking :: (Int, Int) -> Int -> [String] -> [((Int,Int),Int)] -> [((Int,Int),Int)]
walkWithLoopChecking pos dir lab prevSteps 
    | not stillInBounds = [(pos,dir)]
    | stuckInALoop = [((-1,-1),-1)]
    | otherwise = (pos,dir) : walkWithLoopChecking newPos newDir lab ((pos,dir):prevSteps)
    where
        stepInfo = step pos dir lab 0
        newPos = Tools.Tuples.elem3Fst stepInfo
        newDir = Tools.Tuples.elem3Snd stepInfo
        stillInBounds = Tools.Tuples.elem3Trd stepInfo
        stuckInALoop = Tools.Tuples.elemday6 (newPos,newDir) prevSteps

isStuck :: [((Int,Int),Int)] -> Bool
isStuck steps = last steps == ((-1,-1),-1)

createLabs :: [String] -> [(Int,Int)] -> [[String]]
createLabs lab paths = [uncurry createNewLab r lab | r <- paths, uncurry canCreateNewLab r lab]

-- cycleTestLabs originalLab, original path -> count of test that resulted in a loop
cycleTestLabs :: (Int, Int) -> Int -> [String] -> [(Int,Int)] -> Int
cycleTestLabs pos dir lab steps = length [True| newLab <- createLabs lab steps, isStuck (walkWithLoopChecking pos dir newLab [])]

run ::  IO()
run = do

    let filePath = "data/Day06/input.txt"
    lab <- parseFile filePath -- (lab !! i) !! j is char at row i column j of lab
 
    let labMaxRow = length lab -1
    let labMaxCol = length (head lab) -1

    let guardLocation = head [(i,j)| i <- [0..labMaxRow], j <- [0..labMaxCol], (lab !! i) !! j == '^']
    let guardFacing = fNorth
    
    print ("Guard starting at row: " ++ show (fst guardLocation) ++ ", col: " ++  show (snd guardLocation))
       
    let stepAll = walk guardLocation guardFacing lab

    -- print ("Guard walked " ++ show (length stepAll) ++ " steps before exiting the area.")
    
    let stepUnique = countUniqueSteps stepAll
    print ("Part 1: Guard covered " ++ show stepUnique ++ " distinct positions.") -- 5318

    -- Part 2
    {-  
        brute force time
        for every step in the list, create a new lab with the next step as an obstacle, see if the guard loops
        loop can be detected if the next step-dir already exists
    -}

    let testLocations = removeDuplicateSteps stepAll
    print ("Found " ++ show (length testLocations) ++ " valid test locations")
    let validObstructions = cycleTestLabs guardLocation guardFacing lab testLocations

    print ("Part 2, found " ++ show validObstructions ++ " valid obstructions." ) -- 1831
