module Day05 where

import Tools.Parsing
import qualified Data.IntMap.Strict as IntMap
import Data.List.Split
import Data.Maybe

listTo2Tuple :: [String] -> (Int,[Int])
listTo2Tuple [x,y] = (read x,[read y])

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
    pageRules == Nothing ||
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

fixOrder :: [Int] -> [Int] -> [Int]
fixOrder badpages ruleset : [1,2,3]
-- TODO: find a page that will be valid then create a list of [validPage]:[fixOrder (badpages with valid removed)]
-- do recursively until badpages is nothing 

run ::  IO()
run = do
    {-
    1.
    could go through each list left to right
    for [a,b,c,d] we would select 'a' then search for any rules for the form b|a, c|a, d|a
    if nothing found the 'a' is correct
    we can then repeat for [b,c,d] etc until we either find a broken rule or we're left with [d]
    
    2.
    alternatively go through the lists right to left
    for every item, look for rules containing that item then check the rules on the remaining list 
    
    
    2 is more efficient 
    -}

    let filePath = "data/Day05/sample.txt"
    fileData <- parseFile filePath

    let fileSplit = span (/= "") fileData

    let rules = fst fileSplit
    let pages = tail (snd fileSplit)

    let parsedRules = parseRules rules
    let parsedPages = [reverse [read v :: Int| v <- splitOn "," p] | p <- pages]

    -- let test = arePagesValid (parsedPages !! 0) parsedRules
    -- print test
    let validMiddles = [listMiddle l| l <- parsedPages, arePagesValid l parsedRules]
    let p1Sum = sum validMiddles
    print ("Part 1 result: " ++ show p1Sum)  -- 5964

    -- part 2:
    let invalidPages = [l| l <- parsedPages, not(arePagesValid l parsedRules)]
    print invalidPages

