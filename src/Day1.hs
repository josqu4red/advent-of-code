module Day1
     ( day1s1
     , day1s2
     ) where

import Data.Char (isDigit)
import Data.List (elemIndex, findIndex, isPrefixOf, reverse, sortBy, tails)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)

-- Step 1

type Line = String

findFirstDigit :: Line -> Maybe Char
findFirstDigit [] = Nothing
findFirstDigit (x:xs)
  | isDigit x = Just x
  | otherwise = findFirstDigit xs

findLineValue :: Line -> Int
findLineValue line =
  let first = fromJust $ findFirstDigit line
      last  = fromJust $ findFirstDigit (reverse line)
      value = [first, last]
  in read value :: Int

day1s1 :: IO ()
day1s1 = do
  content <- readFile "data/day1"
  let ls = lines content
  print $ sum (map findLineValue ls)

-- Step 2

type WordPos = (Maybe Int, String)

wordList = [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" ]
revWordList = map reverse wordList
digitList = [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
allList = wordList ++ digitList
revAllList = revWordList ++ digitList

wordVal :: String -> Int
wordVal word = fromJust (elemIndex word allList) `mod` 10

findWordInLine :: String -> Line -> Maybe Int
findWordInLine word line = findIndex (isPrefixOf word) (tails line)

findWordsInLine :: [String] -> Line -> [WordPos]
findWordsInLine words line =
  let positions = map (`findWordInLine` line) words
  in zip positions words

findFirstWordInLine :: [String] -> Line -> String
findFirstWordInLine words line =
  let
    wordPos = findWordsInLine words line
    filtered = filter (isJust . fst) wordPos
    sorted = sortBy (comparing fst) filtered
    (_,value) = head sorted
  in value

findLineValue2 :: Line -> Int
findLineValue2 line =
  let first = findFirstWordInLine allList line
      last  = reverse (findFirstWordInLine revAllList (reverse line))
  in wordVal first * 10 + wordVal last

day1s2 :: IO ()
day1s2 = do
  content <- readFile "data/day1"
  let ls = lines content
  print $ sum (map findLineValue2 ls)

