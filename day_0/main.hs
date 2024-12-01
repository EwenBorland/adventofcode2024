main ::  IO()
main = do
    filecontent <- readFile "day_0/input.txt"
    putStrLn "reading file"

    -- print(map readstring(words(filecontent))))
    -- print . words $ filecontent
    let linesofFile = lines filecontent
    print linesofFile

    putStrLn "did it work?"
