{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Y2024.D01 where

import Common
import Data.List (sort, genericLength)

getLists :: String -> ([Integer], [Integer])
getLists = unzip . map ((\[a,b] -> (a,b)) . map read . words) . lines

totalDist :: String -> Integer
totalDist contents = sum $ zipWith diff (sort xs) (sort ys)
  where
    diff x y = abs $ x - y
    (xs, ys) = getLists contents

similarity :: String -> Integer
similarity contents = sum $ calcSim <$> xs
  where
     (xs, ys) = getLists contents
     calcSim x = x * occurence
       where occurence = genericLength $ filter (== x) ys
