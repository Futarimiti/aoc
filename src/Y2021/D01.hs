module Y2021.D01 where

import Common

type Depth = Int

largerTimes :: [Depth] -> Int
largerTimes = count id . (zipWith (<) <*> tail)

largerWindowTimes :: [Depth] -> Int
largerWindowTimes depths = largerTimes $ zipWith3 sum3 depths (drop 1 depths) (drop 2 depths)
  where sum3 a b c = a + b + c
