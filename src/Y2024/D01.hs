{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2024.D01 where

import Common
import Data.List (sort, transpose)
import Data.Map ((!))

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
