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

run ::  IO()
run = do
    let filePath = "data/Day04/input.txt"
    rows <- parseFile filePath

    let rowLength = length (head rows)

    let cols = [transposeString i rows | i <- [0..rowLength-1]]
    
    let rows' = [reverse r| r <- rows]
    let cols' = [reverse c| c <- cols]

    let pRow = rotatePRows rows
    let pCol = [reverse r| r <- rotatePRows rows']

    let pRow' = [reverse r| r <- pRow]
    let pCol' = [reverse c| c <- pCol]

    -- let index = 2
    -- let maxCol = length (head rows) -1
    -- let maxRow = length rows -1
    -- let test = getFromColIndex rows index
    -- print test
    -- print maxCol
    -- print maxRow
    -- print [(maxCol-index)..maxRow]
    -- let testy = zip (reverse [(maxCol-index)..maxRow]) [index..maxCol]
    -- print testy

    let rowsMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- rows]
    let rowsMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- rows']
    let colsMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- cols]
    let colsMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- cols']
    let pRowMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pRow]
    let pRowMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pRow']
    let pColMatches = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pCol]
    let pColMatches' = sum [ length ( getAllTextMatches (r =~ "XMAS") :: [String] )| r <- pCol']

    print ("Part 1 count: " ++ show (rowsMatches + rowsMatches' + colsMatches + colsMatches' + pRowMatches + pRowMatches' + pColMatches + pColMatches') ) -- 2344
    -- print rowsMatches 
    -- print rowsMatches' 
    -- print colsMatches 
    -- print colsMatches' 
    -- print pRowMatches 
    -- print pRowMatches' 
    -- print pColMatches 
    -- print pColMatches'

    -- print rows 
    -- print rows' 
    -- print cols 
    -- print cols' 
    -- print pRow 
    -- print pRow' 
    -- print pCol 
    -- print pCol'