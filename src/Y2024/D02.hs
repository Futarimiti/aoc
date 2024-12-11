{-# LANGUAGE NoOverloadedLists #-}

module Y2024.D02 () where

import Common

instance AOC 2024 2 where
  type Input 2024 2 = [[Int]]
  parse :: [String] -> Input 2024 2
  parse = fmap (fmap read . words)

  type Output1 2024 2 = Integer
  part1 :: Input 2024 2 -> Output1 2024 2
  part1 = count safe

  type Output2 2024 2 = Integer
  part2 :: Input 2024 2 -> Output2 2024 2
  part2 = count tolerable


safe :: [Int] -> Bool
safe = liftA2 (||) inc dec
  where
    inc = all (`elem` [1, 2, 3]) . diffs
    dec = all (`elem` [-1, -2, -3]) . diffs

    diffs :: [Int] -> [Int]
    diffs = zipWith (-) <*> drop 1

tolerable :: [Int] -> Bool
tolerable = liftA2 (||) safe (any safe . remove1)
  where remove1 xs = [take n xs ++ drop (n + 1) xs | n <- [0 .. length xs - 1]]
