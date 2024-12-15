module Tools.Parsing where
    
import Data.Char

parseFile :: String -> IO [String]
parseFile path = do
    content <- readFile path
    let ls = lines content
    print ( "Parsed " ++ show ( length ls ) ++ " lines from '" ++ path ++ "'" )
    return ls

stringsToInts :: [String] -> [[Int]]
stringsToInts f = [[digitToInt c | c <- s] | s <- f]