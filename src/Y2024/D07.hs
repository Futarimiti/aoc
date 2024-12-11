{-# LANGUAGE NoOverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Y2024.D07 () where

import Common
import NE
import L
import Read

feasible :: (Num a, Eq a)
         => [a -> a -> a]  -- | possible operations
         -> NonEmpty a     -- | operands, with fixed order
         -> a              -- | expected result
         -> Bool           -- | feasible?
feasible _ (x:|[]) res = x == res
feasible ops (x:|y:xs) res = not $ null [() | op <- ops, feasible ops (op x y:|xs) res]

instance AOC 2024 7 where
  type Input 2024 7 = [(Integer, NonEmpty Integer)]
  readp = sepBy1 line newline
    where
      newline = char '\n'
      line :: ReadP (Integer, NonEmpty Integer)
      line = do
        res <- uint
        _ <- string ": "
        xs <- sepBy1 uint (char ' ')
        pure (res, fromList xs)  -- safe

  type Output1 2024 7 = Integer
  part1 :: Input 2024 7 -> Output1 2024 7
  part1 = sum . fmap fst . mfilter good
    where good (res, xs) = feasible [(+), (*)] xs res

  type Output2 2024 7 = Integer
  part2 :: Input 2024 7 -> Output2 2024 7
  part2 = sum . fmap fst . mfilter good
    where good (res, xs) = feasible [(+), (*), cat] xs res
          cat x y = read $ show x ++ show y

