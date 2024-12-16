module Day11 where

import Tools.Parsing
import Data.List.Split
import Data.MemoTrie

splitStone :: Int -> [Int]
splitStone stone = [l,r]
    where
        stoneAsString = show stone
        splitIndex = length stoneAsString `div` 2
        l = read (take splitIndex stoneAsString)
        r = read (drop splitIndex stoneAsString)

change :: Int -> [Int]
change stone
    | stone == 0 = [1]
    | even (length (show stone)) = splitStone stone
    | otherwise = [stone * 2024]

blink :: [Int] -> Int -> [Int]
blink stones 0 = stones
blink stones n = blink (concat [change stone | stone <- stones]) (n-1)

blinkSingle :: Int -> Int -> Int
blinkSingle = memo2 blinkSingle'
    where
        blinkSingle' _ 0 = 1
        blinkSingle' stone n = sum [blinkSingle stone' (n-1) | stone' <- change stone]

blinkp2 :: [Int] -> Int -> Int
blinkp2 stones n = sum [blinkSingle stone n | stone <- stones]

run ::  IO()
run = do
    let filePath = "data/Day11/input.txt"
    fileData <- parseFile filePath
    let stones = [read i :: Int | i <- splitOn " " (head fileData)]

    let part1Stones = blink stones 25
    print ("Day 11 Part 1 number of stones: " ++ show(length part1Stones))

    let part2Ans = blinkp2 stones 75
    print ("Day 11 Part 2 number of stones: " ++ show part2Ans)
