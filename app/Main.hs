module Main where

import System.Environment
import Text.Printf
import Day00
import Day01
import Day02
import Day03

runDay :: Int  -> IO ()
runDay n
  | n == 0 = Day00.run 
  | n == 1 = Day01.run 
  | n == 2 = Day02.run 
  | n == 3 = Day03.run
  | otherwise = print ("Solution not implemented for day " ++ show n)

main :: IO ()
main = do
  args <- getArgs
  let dayI = read (head args) :: Int
  print ("Running solution for day " ++ (printf "%02d" dayI))

  runDay dayI
