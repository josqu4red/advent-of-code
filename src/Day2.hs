{-# LANGUAGE OverloadedStrings #-}

module Day2
     ( day2s1
     , day2s2
     ) where

import Data.List.Split (splitOn)

-- Step 1

data Draw = Draw { redz, greenz, bluez :: Int }
  deriving Show

data Game = Game { num :: Int, draws :: [Draw] }
  deriving Show

emptyDraw :: Draw
emptyDraw = Draw 0 0 0

maxDraw :: Draw -> Draw -> Draw
maxDraw d1 d2 =
  Draw (max (redz d1) (redz d2)) (max (greenz d1) (greenz d2)) (max (bluez d1) (bluez d2))

mergeDrawsKeepMax :: [Draw] -> Draw
mergeDrawsKeepMax = foldl maxDraw emptyDraw

possible :: Game -> Int
possible game =
  let draw = mergeDrawsKeepMax (draws game)
  in if redz draw <= 12 && greenz draw <= 13 && bluez draw <= 14
     then num game
     else 0

parseGame :: String -> Game
parseGame str =
  let
    (game:rest) = splitOn ": " str
    num = parseGameNum game
    draws = parseDraws (head rest)
  in Game num draws

parseGameNum :: String -> Int
parseGameNum ('G':'a':'m':'e':' ' : num) = read num :: Int

parseDraws :: String -> [Draw]
parseDraws ds =
  let draws = splitOn "; " ds
  in map parseDraw draws

parseDraw :: String -> Draw
parseDraw d =
  let
    draw = splitOn ", " d
    colors = map parseColor draw
  in mergeDrawsKeepMax colors

parseColor :: String -> Draw
parseColor s =
  let (x:xs) = splitOn " " s
  in case head xs of
    "red" -> Draw (read x :: Int) 0 0
    "green" -> Draw 0 (read x :: Int) 0
    "blue" -> Draw 0 0 (read x :: Int)

day2s1 :: IO ()
day2s1 = do
  content <- readFile "data/day2"
  let ls = lines content
  print $ sum (map (possible . parseGame) ls)

-- Step 2

powerOfMinNeeded :: Game -> Int
powerOfMinNeeded game =
  let d = mergeDrawsKeepMax (draws game)
  in redz d * greenz d * bluez d

day2s2 :: IO ()
day2s2 = do
  content <- readFile "data/day2"
  let ls = lines content
  print $ sum (map (powerOfMinNeeded . parseGame) ls)
