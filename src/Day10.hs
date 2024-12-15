module Day10 where

import Tools.Parsing
import Data.List

inBounds :: (Int, Int) -> [[Int]] -> Bool
inBounds (posR,posC) topoMap= (posR >= 0) && (posR < length topoMap) && (posC >= 0) &&(posC < length (head topoMap))

validGradient :: (Int, Int) -> (Int, Int) -> [[Int]]-> Bool
validGradient (pos1R,pos1C) (pos2R,pos2C) topoMap = ((topoMap !! pos2R) !! pos2C) - ((topoMap !! pos1R) !! pos1C) == 1

followPath :: (Int, Int) -> [[Int]] -> [(Int,Int)] -> [(Int,Int)]
followPath (posR,posC) topoMap peaks = 
    if valueAtPos == 9 then peaks ++[(posR,posC)]
    else peaks ++ newPeaks
    where
        valueAtPos = (topoMap !! posR) !! posC
        posUp = (posR - 1, posC)
        posDown = (posR + 1, posC)
        posLeft = (posR, posC - 1)
        posRight = (posR, posC + 1)
        nextSteps = [posUp,posDown,posLeft,posRight]
        newPeaks = concat [followPath newPos topoMap peaks| newPos <- nextSteps, inBounds newPos topoMap, validGradient (posR,posC) newPos topoMap]

run ::  IO()
run = do
    let filePath = "data/Day10/input.txt"
    fileData <- parseFile filePath
    -- let fileData = ["0123","1234","8765","9876"]
    let topoMap = stringsToInts fileData
    
    let trailHeads = [(ri,ci)| ri<-[0..(length topoMap - 1)], ci <- [0..(length (head topoMap) - 1)], (topoMap !! ri) !! ci == 0]
    -- print trailHeads

    let scores = [followPath tHead topoMap [] | tHead <- trailHeads]
    let p1Ans = length (concat [nub s | s <- scores])
    -- print scores
    print ("Day 10 Part 1 scores: " ++ show p1Ans)

    let p2Ans = length (concat scores)
    print ("Day 10 Part 2 ratings: " ++ show p2Ans)
