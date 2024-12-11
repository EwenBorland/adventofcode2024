module Day09 where

import Tools.Parsing
import Data.Char

parseInput :: [Char] -> Int -> [Int]
parseInput [] _ = []
parseInput input depth 
    | length input == 1 = replicate (digitToInt (input !! 0)) depth
    | otherwise = replicate (digitToInt (input !! 0)) depth ++ replicate (digitToInt (input !! 1)) (-1) ++ parseInput (drop 2 input) (depth + 1)

sortDisk :: [Int] -> Int -> Int -> [Int]
sortDisk disk lpos rpos
    | lpos > rpos = []
    | otherwise = 
        if disk !! lpos /= -1 
        then disk !! lpos : sortDisk disk (lpos + 1) rpos
        else last rposthing : sortDisk disk (lpos + 1) (rpos - length rposthing) 
        where
            rposthing = reverseUntilBreak disk rpos
         
reverseUntilBreak :: [Int] -> Int-> [Int]
reverseUntilBreak disk rpos
    | (disk !! rpos) /= -1 = [disk !! rpos]
    | otherwise = (disk !! rpos) : reverseUntilBreak disk (rpos -1)

run ::  IO()
run = do
    let filePath = "data/Day09/input.txt"
    fileData <- parseFile filePath
    let disk = parseInput (head fileData) 0
    let sortedDisk = sortDisk disk 0 (length disk -1)
    let diskChecksum = sum (zipWith (*) sortedDisk [0..])
    print ("Part 1 checksum: " ++ show diskChecksum)