module Tools.Parsing where

parseFile :: String -> IO [String]
parseFile path = do
    content <- readFile path
    let ls = lines content
    print ( "Parsed " ++ show ( length ls ) ++ " lines from '" ++ path ++ "'" )
    return ls