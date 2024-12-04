module Day03 where

import Data.List
import Tools.Input

findInstructionCandidites :: String -> String -> [Int]
findInstructionCandidites instruction memory = findIndices (isPrefixOf instruction) (tails memory)

evalLine :: String -> IO Int
evalLine memory = do
    let instructionIndices = findInstructionCandidites iMul memory
    -- for each instruction
    -- go by index, if value is not a num,',',or')' then instruction is invalid
    -- save nums to a string, when we ge to the comma then start saving to a second string
    return 1

iMul :: String
iMul = "mul("

run ::  IO()
run = do
    let filePath = "data/day03/sample.txt"
    fileData <- Tools.Input.parseFile filePath
    let evalLines = [evalLine line | line <- fileData]
    let mulIndices = findInstructionCandidites iMul (head fileData)
    print (show mulIndices)

    print iMul