module Y2021.D01 () where

import Common

type Depth = Integer

largerTimes :: [Depth] -> Integer
largerTimes = count id . (zipWith (<) <*> drop 1)

largerWindowTimes :: [Depth] -> Integer
largerWindowTimes depths = largerTimes $ zipWith3 sum3 depths (drop 1 depths) (drop 2 depths)
  where sum3 a b c = a + b + c

instance AOC 2021 1 where
  type Input 2021 1 = [Depth]
  parse :: [String] -> Input 2021 1
  parse = fmap read

  type Output1 2021 1 = Integer
  part1 :: Input 2021 1 -> Output1 2021 1
  part1 = largerTimes

  type Output2 2021 1 = Integer
  part2 :: Input 2021 1 -> Output2 2021 1
  part2 = largerWindowTimes

