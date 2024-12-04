module Day00 where

run ::  IO()
run = do
    filecontent <- readFile "data/day00/input.txt"
    putStrLn "reading file"

    -- print(map readstring(words(filecontent))))
    -- print . words $ filecontent
    let linesofFile = lines filecontent
    print linesofFile

    putStrLn "did it work?"
