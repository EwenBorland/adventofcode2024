module Tools.Input where

import Data.List.Split

parseFile :: String -> IO [String]
parseFile path = do
    content <- readFile path
    let ls = lines content
    print ( "Parsed " ++ show ( length ls ) ++ " lines from '" ++ path ++ "'" )
    return ls


parseToIntLists :: [String] -> [[Int]]
parseToIntLists fileAsStrings = [[read i :: Int| i <- (splitOn "   " l)]| l <- fileAsStrings]