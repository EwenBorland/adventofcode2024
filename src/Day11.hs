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

change' :: Int -> [Int]
change' = memo change 

blink :: [Int] -> Int -> [Int]
blink stones 0 = stones
blink stones n = blink (concat [change stone | stone <- stones]) (n-1)

blink' :: [Int] -> Int -> [Int]
blink' stones 0 = stones
blink' stones n = blink' (concat [change' stone | stone <- stones]) (n-1)

run ::  IO()
run = do
    let filePath = "data/Day11/input.txt"
    fileData <- parseFile filePath
    let stones = [read i :: Int | i <- splitOn " " (head fileData)]
    -- print stones
    -- let stones = [125, 17]
    let part1Stones = blink stones 25
    print ("Day 11 Part 1 number of stones: " ++ show(length part1Stones))

    let part2Stones = blink' stones 75
    print ("Day 11 Part 2 number of stones: " ++ show(length part2Stones))
    -- too slow, need to make a function that takes a single stone and blink count then returns a list? count?
    -- should make the memoization more effective but not cause the memory usage to rocket like it does when memoizing blink'
