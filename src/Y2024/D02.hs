{-# LANGUAGE NoOverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Y2024.D02 where

import Common

safe :: [Int] -> Bool
safe = liftA2 (||) inc dec
  where
    inc = all (`elem` [1, 2, 3]) . diffs
    dec = all (`elem` [-1, -2, -3]) . diffs

    diffs :: [Int] -> [Int]
    diffs = zipWith (-) <*> tail

tolerable :: [Int] -> Bool
tolerable = liftA2 (||) safe (any safe . remove1)
  where remove1 xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]
