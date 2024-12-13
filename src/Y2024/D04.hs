module Y2024.D04 () where

import Common

import L (transpose, isPrefixOf, tails, every)

instance AOC 2024 4 where
  type Input 2024 4 = [[Char]]
  parse :: [String] -> Input 2024 4
  parse = id

  type Output1 2024 4 = Int
  part1 :: Input 2024 4 -> Output1 2024 4
  part1 = liftA2 (+) (search "XMAS") (search "SAMX")

  type Output2 2024 4 = Int
  part2 :: Input 2024 4 -> Output2 2024 4
  part2 = searchX


horizontals, verticals, diagonals :: [[a]] -> [[a]]
horizontals = id
verticals = transpose
diagonals = liftA2 (++) diagonal1 (diagonal1 . map reverse)
  where
    diagonal1 = liftA2 (++)
      (transpose . zipWith drop [0..])
      (transpose . zipWith drop [1..] . rotate180)
    rotate90 = reverse . transpose
    rotate180 = rotate90 . rotate90

search :: Eq a => [a] -> [[a]] -> Int
search pat board = sum $ map (occurrence pat) allSeqs
  where
    allSeqs = horizontals board ++ verticals board ++ diagonals board
    occurrence pat' xs = length $ filter (pat' `isPrefixOf`) (tails xs)

-- | get all 3x3 squares
all3x3 :: [[a]] -> [[[a]]]
all3x3 board = do
  ranks's <- every 3 ranks
  [xs, ys, zs] <- pure $ map (every 3) ranks's
  zipWith3 (\x y z -> [x, y, z]) xs ys zs
  where ranks = board

searchX :: [[Char]] -> Int
searchX = count isXMAS . all3x3
  where
    isXMAS :: [[Char]] -> Bool
    isXMAS = \case
      [ ['M', _, 'M'], [_, 'A', _], ['S', _, 'S'] ] -> True
      [ ['M', _, 'S'], [_, 'A', _], ['M', _, 'S'] ] -> True
      [ ['S', _, 'M'], [_, 'A', _], ['S', _, 'M'] ] -> True
      [ ['S', _, 'S'], [_, 'A', _], ['M', _, 'M'] ] -> True
      _ -> False
