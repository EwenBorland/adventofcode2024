module Day01 where

import Data.List
import Data.List.Split
import Tools.Parsing

-- splitTupleToLists :: [Int,Int] -> [Int] -> [Int] -> ([Int],[Int])
-- -- splitTupleToLists [a,b] l1 l2 = (l1 ++ [a], l2 ++ [b])
-- splitTupleToLists :: [[String]] -> ([Int],[Int])
-- splitTupleToLists [v:vs] = ([read (v !! 0)],[read (v !! 1)])
-- [
--  ["4","5"],
--  ["3","2"],
-- ]

countInt :: Int -> [Int] -> Int
countInt val li = length [1 | v <- li, v == val]

run ::  IO()
run = do
    let filePath = "data/Day01/input.txt"
    fileData <- parseFile filePath

    -- print fileAsIntLists
    
    -- parse file into two lists
    -- order lists small -> large
    -- p1: iterate over list indices, summing difference
    -- p2: count unique in list 2, save as a map/dictionary?
    -- p2: iterate over first list values, sum product with list2 map
    

    -- let list1 = []

    -- let list2 = []
    
    let fileSplitToInts = [map (read) (splitOn "   " l) :: [Int] | l<- fileData]
    let rows = fileSplitToInts
    let cols = transpose rows
    -- print (rows)
    -- print (cols)

    let colsSorted = [sort l :: [Int] | l <- cols]


    -- print (colsSorted)
    -- print (list1)
    -- part 1

    let distances = [ abs (((colsSorted !! 1) !! i) - ((colsSorted !! 0) !! i)) | i <- [0..(length (colsSorted !! 0)-1)]]
    -- print distances
    let totalDistance = sum distances

    print ("Part 1 Distance: " ++ show totalDistance) -- 1603498

    -- part 2

    let uniquesInCol1 = [(head l, length l)| l <- group (head colsSorted)]

    -- print uniquesInCol1
    let uniquesInBoth = [(v,c,countInt v (colsSorted !! 1))| (v,c) <- uniquesInCol1]
    -- print uniquesInBoth

    let similaritySum = sum [v*c0*c1| (v, c0, c1) <- uniquesInBoth]
    print ("Part 2 similarity score: " ++ show similaritySum) -- 25574739
    