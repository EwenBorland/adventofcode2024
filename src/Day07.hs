module Day07 where

import Tools.Parsing
import Data.List.Split

generateOps :: Int -> [Int->Int->Int] -> [[Int->Int->Int]]
generateOps i opList
    | i == 0 = []
    | otherwise = mapM (const opList) [1..i]

parseResultArgs :: String -> (Int, [Int])
parseResultArgs row = (res, args)
    where
        rowSplit = splitOn ": " row
        res = read (head rowSplit)
        args = [read a| a <- splitOn " " (last rowSplit)]

calcArgsOps :: [Int] -> [Int->Int->Int] -> Int -> Int -> Int
calcArgsOps _ [] acc _ = acc
calcArgsOps args ops 0 lim= calcArgsOps (tail (tail args)) (tail ops) ((ops !! 0) (args !! 0) (args !! 1)) lim
calcArgsOps args ops acc lim
    | acc > lim = acc -- exit early
    | otherwise = calcArgsOps (tail args) (tail ops) ((ops !! 0) acc (args !! 0)) lim

possibleResultArgs :: Int -> [Int] -> [Int->Int->Int] -> Bool
possibleResultArgs res args opList = isPossible 
    where
        operations = generateOps (length args -1) opList
        pots = [calcArgsOps args op 0 res|op <- operations]
        isPossible = res `elem` pots

concatInts :: Int -> Int -> Int
concatInts a b = read (show a ++ show b)

run ::  IO()
run = do
    let filePath = "data/Day07/input.txt"
    fileData <- parseFile filePath  

    let parsedRows = [parseResultArgs r| r <- fileData]
    -- print parsedRows

    let possibleResults = [fst r| r <- parsedRows, uncurry possibleResultArgs r [(*),(+)]]
    let sumPossibleResults = sum possibleResults

    print ("Part 1: Sum of possible values: " ++ show sumPossibleResults) -- 2941973819040

    let p2possibleResults = [fst r| r <- parsedRows, uncurry possibleResultArgs r [(*),(+),concatInts]]
    let p2sumPossibleResults = sum p2possibleResults

    print ("Part 2: Sum of possible values: " ++ show p2sumPossibleResults) -- 249943041417600

