module Day03 where

import Tools.Parsing
import Text.Regex.TDFA

regexPatMul :: String
regexPatMul = "mul\\([0-9]+,[0-9]+\\)"
regexPatNum :: String
regexPatNum = "[0-9]+"
regexPatDo :: String
regexPatDo = "do\\(\\)"
regexPatDont :: String
regexPatDont = "don't\\(\\)"

regexPatPart2 :: String
regexPatPart2 = regexPatMul ++ "|" ++ regexPatDo ++ "|" ++ regexPatDont


evalMul :: String -> Int
evalMul s = a*b
    where 
        a = read (head nums)
        b = read (last nums) 
        nums = getAllTextMatches (s =~ regexPatNum) :: [String]

isEnabled :: Int -> [(Int, Bool)] -> Bool
isEnabled index flipIndices = last (True:[enabled| (findex, enabled) <- flipIndices, index > findex])

evalLineP1 :: String -> IO Int
evalLineP1 memory = do
    let matches = getAllTextMatches (memory =~ regexPatMul) :: [String]
    -- print matches

    let evals = [evalMul s | s <- matches]

    -- print evals

    return (sum evals)

evalLineP2 :: String -> IO Int
evalLineP2 memory = do
    print regexPatPart2
    let matches = getAllTextMatches (memory =~ regexPatPart2) :: [String]
    print matches
    
    let flipIndices = [(i, matches !! i == "do()") | i <- [0..(length matches -1)], matches !! i == "do()" || matches !! i == "don't()"]
    print flipIndices

    let evals = [evalMul (matches!!i) | i <- [0..(length matches -1)], ((matches!!i) /="don't()") && ((matches!!i) /="do()") && isEnabled i flipIndices]

    return (sum evals)

run ::  IO()
run = do
    let filePath = "data/day03/input.txt"
    fileData <- Tools.Parsing.parseFile filePath

    p1Solution <- evalLineP1 (head fileData)

    print ("Part 1 result: " ++ show p1Solution)  -- 170778545
    
    p2Solution <- evalLineP2 (head fileData)

    print ("Part 2 result: " ++ show p2Solution)  -- 82868252