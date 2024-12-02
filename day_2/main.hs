lineToInts :: String -> [Int]
lineToInts str = map read ( words str )

isDescending :: [Int] -> Bool
isDescending ints = if ((length ints) - 1) == (length [1 | i <- [0..((length ints) - 2)] , ints !! i > ints !! (i+1), 1 <= (ints !! i - ints !! (i+1)) , (ints !! i - ints !! (i+1))<= 3 ])
    then True
    else False

isAscending :: [Int] -> Bool
isAscending ints = if ((length ints) - 1) == (length [1 | i <- [0..((length ints) - 2)] , ints !! (i+1) > ints !! i, 1 <= (ints !! (i+1) - ints !! i) , (ints !! (i+1) - ints !! i) <= 3 ])
    then True
    else False

isAscendingOrDescending :: [Int] -> Bool
isAscendingOrDescending ints = isAscending ints || isDescending ints


main ::  IO()
main = do
    filecontent <- readFile "day_2/input.txt"

    let linesofFile = lines filecontent
    -- print linesofFile
    let linesAsInts = [lineToInts line | line <- linesofFile]
    -- print linesAsInts
    print ( "We have " ++ show ( length linesAsInts ) ++ " lines" )
    print ( "The first line has " ++ show ( length $ linesAsInts !! 0) ++ " elements" )

    --print [isAscending l| l <- linesAsInts]

    -- print (isAscending [1,2,3])
    -- print (isAscending [1,2,3,2])
    -- print (isDescending [3,2,1])
    -- print (isDescending [3,2,1,2])

    let safeReports1 = sum [1| l <- linesAsInts, isAscendingOrDescending l]
    
    print ( "part 1 Safe reports: " ++ show safeReports1)

    let safeReports2 = sum [1|l <- linesAsInts, isAscendingOrDescending l || (any (==1) [1|popIndex <- [0..((length l) - 1)], isAscendingOrDescending (let (ys,zs) = splitAt popIndex l in ys ++ (tail zs)) ])]
    
    print ( "part 2 Safe reports: " ++ show safeReports2)
    