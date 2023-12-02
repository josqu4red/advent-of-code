module Main where

import System.IO
import Day1

main :: IO ()
main = do
  content <- readFile "data/day1"
  let ls = lines content
  -- step 1
  print $ compute ls
  -- step 2
  print $ compute2 ls
