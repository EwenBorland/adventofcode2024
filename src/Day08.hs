module Day08 where

import Tools.Parsing
import qualified Data.Map as Map
import Data.Maybe
import Data.List

parseAntennas :: [String] -> Map.Map Char [(Int,Int)]
parseAntennas antennamap = Map.fromListWith (++) [(char, [(ri,ci)])| ri <- [0..(length antennamap-1)], ci <- [0..(length (head antennamap) -1)],
    let char = (antennamap !! ri) !! ci,
    char /= '.']

inBounds:: (Int, Int) -> Int -> Int -> Bool
inBounds node maxRow maxCol = (fst node >= 0) && (fst node <= maxRow) && (snd node >= 0) && (snd node <= maxCol)

findAntinodes :: (Int,Int) -> (Int,Int) -> Int -> Int -> [(Int,Int)]
findAntinodes nodeA nodeB maxRow maxCol = [n | n <- antinodes, inBounds n maxRow maxCol]
    where 
        rowDist = fst nodeA - fst nodeB
        colDist = snd nodeA - snd nodeB
        antinodes = [(fst nodeA + rowDist, snd nodeA + colDist), (fst nodeB - rowDist, snd nodeB - colDist)] 

findAntinodes' :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
findAntinodes' allNodes maxRow maxCol = concat [uncurry findAntinodes pair maxRow maxCol | pair <- nodePairs]
    where
        nodePairs = pairs allNodes

findAllAntinodes :: Map.Map Char [(Int,Int)] -> Int -> Int -> Map.Map Char [(Int,Int)]
findAllAntinodes antennas maxRow maxCol = Map.fromListWith (++) [(freq, aNodes)| freq <- Map.keys antennas,
    let nodes = Data.Maybe.fromJust (Map.lookup freq antennas),
    let aNodes = findAntinodes' nodes maxRow maxCol ]

findAntinodes2 :: (Int,Int) -> (Int,Int) -> Int -> Int -> [(Int,Int)]
findAntinodes2 nodeA nodeB maxRow maxCol = [n | n <- antinodes, inBounds n maxRow maxCol]
    where 
        rowDist = fst nodeA - fst nodeB
        colDist = snd nodeA - snd nodeB
        antinodesUp = [(fst nodeB - (rowDist*i), snd nodeB - (colDist*i)) | i <- [0..(max maxRow maxCol)]] 
        antinodesDown = [(fst nodeA + rowDist*i, snd nodeA + colDist*i) | i <- [0..(max maxRow maxCol)]] 
        antinodes = antinodesUp ++ antinodesDown

findAntinodes2' :: [(Int,Int)] -> Int -> Int -> [(Int,Int)]
findAntinodes2' allNodes maxRow maxCol = concat [uncurry findAntinodes2 pair maxRow maxCol | pair <- nodePairs]
    where
        nodePairs = pairs allNodes

findAllAntinodes2 :: Map.Map Char [(Int,Int)] -> Int -> Int -> Map.Map Char [(Int,Int)]
findAllAntinodes2 antennas maxRow maxCol = Map.fromListWith (++) [(freq, aNodes)| freq <- Map.keys antennas,
    let nodes = Data.Maybe.fromJust (Map.lookup freq antennas),
    let aNodes = findAntinodes2' nodes maxRow maxCol ]

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

run ::  IO()
run = do
    let filePath = "data/Day08/input.txt"
    fileData <- parseFile filePath
    
    let maxRow = length fileData -1
    let maxCol = length (head fileData) -1
    let antennaLocations = parseAntennas fileData
    -- print antennaLocations


    let antiNodes = findAllAntinodes antennaLocations maxRow maxCol
    let allAntiNodes = concat (Map.elems antiNodes)
    let allUniqueAntiNodes = nub allAntiNodes
    let count = length allUniqueAntiNodes
    print ("Part 1: found " ++ show count ++ " antinodes") -- 303

    let antiNodes2 = findAllAntinodes2 antennaLocations maxRow maxCol
    let allAntiNodes2 = concat (Map.elems antiNodes2)
    let allUniqueAntiNodes2 = nub allAntiNodes2
    let count2 = length allUniqueAntiNodes2
    print ("Part 2: found " ++ show count2 ++ " antinodes") -- 1045