module Main where

import System.Environment
import Text.Printf
import Day00
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
--endofimports

runDay :: Int  -> IO ()
runDay n
  | n == 0 = Day00.run 
  | n == 1 = Day01.run 
  | n == 2 = Day02.run 
  | n == 3 = Day03.run
  | n == 4 = Day04.run
  | n == 5 = Day05.run
  | n == 6 = Day06.run
  | n == 7 = Day07.run
  | otherwise = print ("Solution not implemented for day " ++ show n)

main :: IO ()
main = do
  args <- getArgs
  let dayI = read (head args) :: Int
  print ("Running solution for day " ++ printf "%02d" dayI)

  runDay dayI
