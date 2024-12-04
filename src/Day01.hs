module Day01 where

import Tools.Input

run ::  IO()
run = do
    let filePath = "data/day01/sample.txt"
    fileData <- parseFile filePath

    let fileAsIntLists = parseToIntLists fileData
    -- print fileAsIntLists

    -- part 1
    