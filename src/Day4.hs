{-# LANGUAGE OverloadedStrings #-}

module Day4
     ( day4s1
     , day4s2
     ) where

import Data.List (partition, tails)
import Data.List.Split (splitOn)

-- Step 1

data Card = Card { winNums, allNums :: [Int] }
  deriving Show

strToInts :: [Char] -> [Int]
strToInts = map read . words

parseCard :: String -> Card
parseCard str =
  let (_:nums:_) = splitOn ": " str
      (win:all:_) = splitOn " | " nums
  in Card (strToInts win) (strToInts all)

winningNumbers :: Card -> [Int]
winningNumbers card =
  let (ok,_) = partition (`elem` winNums card) (allNums card)
  in ok

listValue :: [Int] -> Int
listValue [] = 0
listValue [x] = 1
listValue (x:xs) = 2 ^ length xs

cardValue :: Card -> Int
cardValue = listValue . winningNumbers

day4s1 :: IO ()
day4s1 = do
  content <- readFile "data/day4"
  let ls = lines content
  print $ sum (map (cardValue . parseCard) ls)

-- Step 2

cards :: [String] -> [Card]
cards = map parseCard

cardValues :: [Card] -> [Int]
cardValues = map cardValue2

cardValue2 :: Card -> Int
cardValue2 = length . winningNumbers

countCards :: [Int] -> Int
countCards [] = 0
countCards (c:cs) =
  case c of
    0 -> 1
    x -> 1 + sum(map countCards (take x (tails cs)))

day4s2 :: IO ()
day4s2 = do
  content <- readFile "data/day4"
  let ls = lines content
  print $ sum(map countCards (tails (cardValues (cards ls))))
