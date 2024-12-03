import Data.List

parseFile :: String -> IO [String]
parseFile path = do
    content <- readFile path
    let ls = lines content
    print ( "Parsed " ++ show ( length ls ) ++ " lines from '" ++ path ++ "'" )
    return ls

findInstructionCandidites :: String -> String -> [Int]
findInstructionCandidites instruction memory = findIndices (isPrefixOf instruction) (tails memory)

evalLine :: String -> IO Int
evalLine memory = do
    let instructionIndices = findInstructionCandidites iMul memory
    -- for each instruction
    -- go by index, if value is not a num,',',or')' then instruction is invalid
    -- save nums to a string, when we ge to the comma then start saving to a second string
    return 1

iMul :: String = "mul("

main ::  IO()
main = do
    let filePath = "day_3/sample.txt"
    fileData <- parseFile filePath
    let evalLines = [evalLine line | line <- fileData]
    let mulIndices = findInstructionCandidites iMul (head fileData)
    print (show mulIndices)

    print iMul