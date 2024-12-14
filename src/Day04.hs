module Day04 where

import Tools.Parsing
import Text.Regex.TDFA

transposeString :: Int -> [String] -> String
transposeString index rows = [s !! index| s <- rows ]

-- rotatePRows will take the input [String], rotate it 45 deg clockwise and return a new [String]
rotatePRows :: [String] -> [String]
rotatePRows rows = [getFromRowIndex rows i| i <- [0..(length rows -1)]] ++ ["split"] ++  [getFromColIndex rows i| i <- [1..(length (head rows)- 1)]]

getFromRowIndex :: [String] -> Int -> String
getFromRowIndex rows index = [(rows !! ri) !! ci| (ri,ci) <- zip (reverse [0..index]) [0..(min index maxCol)]]
    where maxCol = length (head rows) -1

getFromColIndex :: [String] -> Int -> String
getFromColIndex rows index = [(rows !! ri) !! ci| (ri,ci) <- zip (reverse [index..maxRow]) [index..maxCol]]
    where 
        maxCol = length (head rows) -1
        maxRow = length rows -1

{-
possibly the least time efficient way to solve part one
might come back and redo this one

a better approach would be to go character by character, checking the adjacent characters
should only require going through the data a single time
-}

doPart1 :: [String] -> Int
doPart1 rows = rowsMatches + rowsMatches' + colsMatches + colsMatches' + pRowMatches + pRowMatches' + pColMatches + pColMatches'
    where
        rowLength = length (head rows)
        cols = [transposeString i rows | i <- [0..rowLength-1]]
        rows' = [reverse r| r <- rows]
        cols' = [reverse c| c <- cols]
        pRow = rotatePRows rows
        pCol = [reverse r| r <- rotatePRows rows']
        pRow' = [reverse r| r <- pRow]
        pCol' = [reverse c| c <- pCol]
        rowsMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- rows]
        rowsMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- rows']
        colsMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- cols]
        colsMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- cols']
        pRowMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pRow]
        pRowMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pRow']
        pColMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pCol]
        pColMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pCol']

isXMAS :: [String] -> Int -> Int -> Bool
isXMAS rows ri ci = d1 && d2
    where
        topLeft = (rows !! (ri - 1)) !! (ci - 1)
        topRight = (rows !! (ri - 1)) !! (ci + 1)
        botLeft = (rows !! (ri + 1)) !! (ci - 1)
        botRight = (rows !! (ri + 1)) !! (ci + 1)
        d1 = (topLeft == 'M' && botRight == 'S') || (topLeft == 'S' && botRight == 'M')
        d2 = (topRight == 'M' && botLeft == 'S') || (topRight == 'S' && botLeft == 'M')


findXMAS :: [String] -> Int
findXMAS rows = length [True | ri <- [1.. length rows -2], ci <- [1.. length (head rows) -2], (rows !! ri) !! ci == 'A', isXMAS rows ri ci]

run ::  IO()
run = do
    let filePath = "data/Day04/input.txt"
    rows <- parseFile filePath

    let part1Ans = doPart1 rows
    print ("Part 1 count: " ++ show part1Ans) -- 2344
    
    
    -- part 2

    let part2Ans = findXMAS rows
    print ("Part 2 count: " ++ show part2Ans) -- 2344