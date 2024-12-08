module Day05 where

import Tools.Parsing
import qualified Data.IntMap.Strict as IntMap
import Data.List.Split
import Data.Maybe

listTo2Tuple :: [String] -> (Int,[Int])
listTo2Tuple [x,y] = (read x,[read y])
listTo2Tuple _ = (0,[0])

ruleToTuple :: String -> (Int, [Int])
ruleToTuple rule = listTo2Tuple rSplit 
    where
        rSplit = splitOn "|" rule

parseRules :: [String] -> IntMap.IntMap [Int]
parseRules rules = IntMap.fromListWith (++) [ruleToTuple r | r <- rules]

pagesNotInRuleset :: [Int] -> [Int] -> Bool
pagesNotInRuleset pages ruleset = 
    null pages ||
    notElem (head pages) ruleset &&
    pagesNotInRuleset (tail pages) ruleset

isPageValid :: Int -> [Int] -> IntMap.IntMap [Int] -> Bool
isPageValid page pages rules = 
    isNothing pageRules ||
    pagesNotInRuleset pages (fromJust pageRules)
    where pageRules = rules IntMap.!? page


arePagesValid :: [Int] -> IntMap.IntMap [Int] -> Bool
arePagesValid pages rules = 
    (length pages < 2) ||
    isPageValid (head pages) pages rules &&
    arePagesValid (tail pages) rules

listMiddle :: [Int] -> Int
listMiddle l =
    l !! midIndex
    where midIndex = (length l -1 ) `div` 2

findValidPages :: [Int] -> IntMap.IntMap [Int] -> [Int]
findValidPages pages rules = [pages !! i| i <- [0.. length pages -1], isPageValid (pages !! i) (take i pages ++ drop (i+1) pages) rules]

fixOrder :: [Int] -> [Int] -> IntMap.IntMap [Int] -> [Int]
fixOrder badpages goodpages ruleset =
    if null badpages then goodpages
    else fixOrder remainingPages (goodpages ++ newGoodPages) ruleset
    where 
        newGoodPages = findValidPages badpages ruleset
        remainingPages = [p | p <- badpages, p `notElem` newGoodPages]

run ::  IO()
run = do

    let filePath = "data/Day05/input.txt"
    fileData <- parseFile filePath

    let fileSplit = span (/= "") fileData

    let rules = fst fileSplit
    let pages = tail (snd fileSplit)

    let parsedRules = parseRules rules
    let parsedPages = [reverse [read v :: Int| v <- splitOn "," p] | p <- pages]


    let validMiddles = [listMiddle l| l <- parsedPages, arePagesValid l parsedRules]
    let p1Sum = sum validMiddles
    print ("Part 1 result: " ++ show p1Sum)  -- 5964

    -- part 2:
    let invalidPages = [l| l <- parsedPages, not(arePagesValid l parsedRules)]
    
    let fixedPages = [fixOrder l [] parsedRules | l<-invalidPages]

    let fixedMiddles = [listMiddle l| l <- fixedPages]
    let p2Sum = sum fixedMiddles

    print ("Part 2 result: " ++ show p2Sum)  -- 4719