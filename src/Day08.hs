module Day08 where

import Tools.Parsing
import qualified Data.Map as Map
import Data.Maybe
import Data.List

parseAntennas :: [String] -> Map.Map Char [(Int,Int)]
parseAntennas antennamap = Map.fromListWith (++) [(char, [(ri,ci)])| ri <- [0..(length antennamap-1)], ci <- [0..(length (head antennamap) -1)],
    let char = (antennamap !! ri) !! ci,
    char /= '.']

findAntinodes :: (Int,Int) -> (Int,Int) -> Int -> Int -> [(Int,Int)]
findAntinodes nodeA nodeB maxRow maxCol = [n|n<-antinodes, (fst n >= 0) && (fst n <= maxRow) && (snd n >= 0) && (snd n <= maxCol)]
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
    print count