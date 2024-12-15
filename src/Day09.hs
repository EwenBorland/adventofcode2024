module Day09 where

import Tools.Parsing
import Tools.Tuples
import Data.Char
import Debug.Trace

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
parseInput2 :: [Char] -> Int -> [(Int, Int, Bool, Bool)]
parseInput2 [] _ = []
parseInput2 input i
    | even i = (i `div` 2, digitToInt (head input), True,True) : parseInput2 (tail input) (i + 1)
    | otherwise = (0 , digitToInt (head input), False,True) : parseInput2 (tail input) (i + 1)

-- sortDisk2 :: [(Int, Int, Bool, Bool)] -> [(Int, Int, Bool, Bool)] ->  Int -> [(Int, Int, Bool, Bool)]
-- sortDisk2 _ updatedDisk 0 = updatedDisk
-- sortDisk2 disk updatedDisk index = sortDisk2 disk newdisk (index -2)
--     where
--         spaceRequired = Tools.Tuples.elem3Snd (disk !! index)
--         spacesAt = [i | i <- [0..(length updatedDisk -1)], not (Tools.Tuples.elem3Trd (updatedDisk !! i)) ,Tools.Tuples.elem3Snd (updatedDisk !! i) >= spaceRequired ]
--         newdisk = 
--             if null spacesAt then updatedDisk
--             else take (head spacesAt) updatedDisk ++ [disk !! index] ++ [(0, Tools.Tuples.elem3Snd (updatedDisk !! head spacesAt) - spaceRequired, False)] ++ drop (head spacesAt +1) updatedDisk

--Debug.Trace.traceShowId 
createUpdatedDisk :: [(Int, Int, Bool, Bool)] -> [(Int, Int, Bool, Bool)] ->  Int -> Int -> Int -> Int -> [(Int, Int, Bool, Bool)]
createUpdatedDisk oldDisk updatedDisk index lengthDiff spaceRequired spaceIndex = newDisk
    where
        
        prefix = take spaceIndex updatedDisk
        movedBlock = [oldDisk !! index]
        remainingSpace = [(0, Tools.Tuples.elem4Snd (updatedDisk !! spaceIndex) - spaceRequired, False,False)]
        shift = if Tools.Tuples.elem4Fth(updatedDisk !! spaceIndex) then 0 else 2
        shift' = if Tools.Tuples.elem4Fth(updatedDisk !! spaceIndex) then 0 else 1
        midIndex = (index + lengthDiff) - (spaceIndex+1) - shift
        midUpToOldBlock = take midIndex (drop (spaceIndex +1) updatedDisk)
        replacedBlock = [(0,Tools.Tuples.elem4Snd (oldDisk !! index), False, False)]
        suffix = drop (index + lengthDiff+1 - shift') updatedDisk
        newDisk = prefix ++ movedBlock ++ remainingSpace ++ midUpToOldBlock ++ replacedBlock ++ suffix


sortDisk2 :: [(Int, Int, Bool, Bool)] -> [(Int, Int, Bool, Bool)] ->  Int -> [(Int, Int, Bool, Bool)]
sortDisk2 _ updatedDisk 0 = updatedDisk
sortDisk2 _ updatedDisk (-1) = updatedDisk
sortDisk2 disk updatedDisk index = sortDisk2 disk newdisk (index -2) 
    where
        lengthDiff = length updatedDisk - length disk
        spaceRequired = Tools.Tuples.elem4Snd (disk !! index)
        spacesAt = [i | i <- [0..(index + lengthDiff - 1)], not (Tools.Tuples.elem4Trd (updatedDisk !! i)) ,Tools.Tuples.elem4Snd (updatedDisk !! i) >= spaceRequired ]
        newdisk = 
            if null spacesAt then updatedDisk
            else createUpdatedDisk disk updatedDisk index lengthDiff spaceRequired (head spacesAt)

printDisk2 :: [(Int, Int, Bool, Bool)] -> [Char]
printDisk2 [] = ""
printDisk2 disk = replicate l value ++ printDisk2 (tail disk)
    where
        dat = head disk
        l = Tools.Tuples.elem4Snd dat
        isNum = Tools.Tuples.elem4Trd dat
        value = if isNum then intToDigit (Tools.Tuples.elem4Fst dat) else '.'

printDisk2' :: [(Int, Int, Bool, Bool)] -> [Int]
printDisk2' [] = []
printDisk2' disk = replicate l value ++ printDisk2' (tail disk)
    where
        dat = head disk
        l = Tools.Tuples.elem4Snd dat
        value = Tools.Tuples.elem4Fst dat

run ::  IO()
run = do
    let filePath = "data/Day09/input.txt"
    fileData <- parseFile filePath
    let disk = parseInput (head fileData) 0
    let sortedDisk = sortDisk disk 0 (length disk -1)
    let diskChecksum = sum (zipWith (*) sortedDisk [0..])
    print ("Part 1 checksum: " ++ show diskChecksum)

    let disk2 = parseInput2 (head fileData) 0
    -- print disk2

    -- let testDisk = [(0,2,True),(0,3,False),(1,3,True),(0,3,False),(2,1,True),(0,3,False),(3,3,True),(0,1,False),(4,2,True),(0,1,False),(5,4,True),(0,1,False),(6,4,True),(0,1,False),(7,3,True),(0,1,False),(8,4,True),(0,0,False),(9,2,True)]      
    -- let testDisk = [(0,2,True),(0,0,False),(9,2,True),(0,1,False),(1,3,True),(7,3,True),(2,1,True)]
    
    -- print ("Input:" ++ printDisk2 disk2)
    let startIndex = if even (length disk2 -1) then  length disk2 - 1 else length disk2 - 2
    let sortedDisk2 = sortDisk2 disk2 disk2 startIndex
    -- print ("Want :" ++ "00992111777.44.333....5555.6666.....8888..")
    -- print ("Got  :" ++ printDisk2 sortedDisk2)
    -- -- print sortedDisk2
    let printableDisk2 = printDisk2' sortedDisk2
    print ("original length: " ++ show (length disk))
    print ("new length: " ++ show (length printableDisk2))
    let diskChecksum2 = sum (zipWith (*) printableDisk2 [0..])
    print ("Part 2 checksum: " ++ show diskChecksum2) -- not 6465407092315, too high. not 12682032250198 either

    {-
    Shelving this for now, the solution for part 2 is far too complicated and has too many edge cases.
    I'll redo this at a later date with a different approach
    Parsing the file into a "00...11...2.333..." format then sorting the string should be simpler than the approach with tuples

    -}