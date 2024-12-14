module Day09 where

import Tools.Parsing
import Tools.Tuples
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


-- part 2
parseInput2 :: [Char] -> Int -> [(Int, Int, Bool)]
parseInput2 [] _ = []
parseInput2 input i
    | even i = (i `div` 2, digitToInt (head input), True) : parseInput2 (tail input) (i + 1)
    | otherwise = (0 , digitToInt (head input), False) : parseInput2 (tail input) (i + 1)

-- sortDisk2 :: [(Int, Int, Bool)] -> [(Int, Int, Bool)] ->  Int -> [(Int, Int, Bool)]
-- sortDisk2 _ updatedDisk 0 = updatedDisk
-- sortDisk2 disk updatedDisk index = sortDisk2 disk newdisk (index -2)
--     where
--         spaceRequired = Tools.Tuples.elem3Snd (disk !! index)
--         spacesAt = [i | i <- [0..(length updatedDisk -1)], not (Tools.Tuples.elem3Trd (updatedDisk !! i)) ,Tools.Tuples.elem3Snd (updatedDisk !! i) >= spaceRequired ]
--         newdisk = 
--             if null spacesAt then updatedDisk
--             else take (head spacesAt) updatedDisk ++ [disk !! index] ++ [(0, Tools.Tuples.elem3Snd (updatedDisk !! head spacesAt) - spaceRequired, False)] ++ drop (head spacesAt +1) updatedDisk

createUpdatedDisk :: [(Int, Int, Bool)] -> [(Int, Int, Bool)] ->  Int -> Int -> Int -> [Int] -> [(Int, Int, Bool)]
createUpdatedDisk oldDisk updatedDisk index lengthChange spaceRequired spacesAt = newDisk
    where
        prefix = take (head spacesAt) updatedDisk
        movedBlock = [oldDisk !! index]
        remainingSpace = [(0, Tools.Tuples.elem3Snd (updatedDisk !! head spacesAt) - spaceRequired, False)]
        midUpToOldBlock = take (index + lengthChange - (head spacesAt +1)) (drop (head spacesAt +1) updatedDisk)
        replacedBlock = [(0,Tools.Tuples.elem3Snd (oldDisk !! index), False)]
        suffix = drop (index + lengthChange ) updatedDisk
        newDisk = prefix ++ movedBlock ++ remainingSpace ++ midUpToOldBlock ++ replacedBlock ++ suffix


sortDisk2 :: [(Int, Int, Bool)] -> [(Int, Int, Bool)] ->  Int -> Int -> [(Int, Int, Bool)]
sortDisk2 _ updatedDisk 0 _ = updatedDisk
sortDisk2 disk updatedDisk index l = sortDisk2 disk newdisk lengthIncrease (index -2)
    where
        spaceRequired = Tools.Tuples.elem3Snd (disk !! index)
        spacesAt = [i | i <- [0..(index + l -1)], not (Tools.Tuples.elem3Trd (updatedDisk !! i)) ,Tools.Tuples.elem3Snd (updatedDisk !! i) >= spaceRequired ]
        lengthIncrease = l + (if null spacesAt then 0 else 1)
        newdisk = 
            if null spacesAt then updatedDisk
            else createUpdatedDisk disk updatedDisk index l spaceRequired spacesAt

printDisk2 :: [(Int, Int, Bool)] -> [Char]
printDisk2 [] = ""
printDisk2 disk = replicate l value ++ printDisk2 (tail disk)
    where
        dat = head disk
        l = Tools.Tuples.elem3Snd dat
        isNum = Tools.Tuples.elem3Trd dat
        value = if isNum then intToDigit (Tools.Tuples.elem3Fst dat) else '.'

run ::  IO()
run = do
    let filePath = "data/Day09/sample.txt"
    fileData <- parseFile filePath
    -- let disk = parseInput (head fileData) 0
    -- let sortedDisk = sortDisk disk 0 (length disk -1)
    -- let diskChecksum = sum (zipWith (*) sortedDisk [0..])
    -- print ("Part 1 checksum: " ++ show diskChecksum)

    -- let disk2 = parseInput2 (head fileData) 0
    -- print disk2

    let testDisk = [(0,2,True),(0,3,False),(1,3,True),(0,3,False),(2,1,True),(0,3,False),(3,3,True),(0,1,False),(4,2,True),(0,1,False),(5,4,True),(0,1,False),(6,4,True),(0,1,False),(7,3,True),(0,1,False),(8,4,True),(0,0,False),(9,2,True)]      

    print (printDisk2 testDisk)
    let startIndex = if even (length testDisk -1) then  length testDisk - 1 else length testDisk - 2
    let sortedDisk2 = sortDisk2 testDisk testDisk startIndex 0
    print (printDisk2 sortedDisk2)
    print "00992111777.44.333....5555.6666.....8888.."