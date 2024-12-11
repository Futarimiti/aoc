{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2024.D01 () where

import Common
import Data.List (sort, transpose)
import Data.Map ((!))

instance AOC 2024 1 where
  type Input 2024 1 = ([Int], [Int])
  parse :: [String] -> Input 2024 1
  parse = tuple2 . transpose . map (map read . words)
    where tuple2 [xs, ys] = (xs, ys)

  type Output1 _ _ = Int
  part1 :: Input 2024 1 -> Output1 2024 1
  part1 (xs, ys) = totalDist xs ys

  type Output2 _ _ = Int
  part2 :: Input 2024 1 -> Output2 2024 1
  part2 = undefined


getLists :: String -> ([Int], [Int])
getLists = tuple2 . transpose . map (map read . words) . lines
  where tuple2 [xs, ys] = (xs, ys)

totalDist :: [Int] -> [Int] -> Int
totalDist xs ys = sum $ (zipWith diff `on` sort) xs ys
  where diff x y = abs $ x - y

similarity :: [Int] -> [Int] -> Int
similarity xs ys = sum $ calcSim <$> xs
  where calcSim x = x * occurence
          where occurence = counts ys ! x
