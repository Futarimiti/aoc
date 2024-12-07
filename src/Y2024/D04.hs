{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Y2024.D04 (search, searchX) where

import Common
import MTL
import Data.List.NonEmpty (NonEmpty (..))
import Data.List ( transpose, tails, isPrefixOf )

type Board a = [[a]]

horizontals, verticals, diagonals :: Board a -> [[a]]
horizontals = id
verticals = transpose
diagonals = liftA2 (++) diagonal1 (diagonal1 . map reverse)
  where
    diagonal1 = liftA2 (++)
      (transpose . zipWith drop [0..])
      (transpose . zipWith drop [1..] . rotate180)
    rotate90 = reverse . transpose
    rotate180 = rotate90 . rotate90

search :: Eq a => [a] -> Board a -> Int
search pat board = sum $ map (occurrence pat) allSeqs
  where
    allSeqs = horizontals board ++ verticals board ++ diagonals board
    occurrence pat xs = length $ filter (pat `isPrefixOf`) (tails xs)

-- | get all 3x3 squares
all3x3 :: Board a -> [[[a]]]
all3x3 board = do
  ranks's <- every 3 ranks
  [xs, ys, zs] <- pure $ map (every 3) ranks's
  zipWith3 (\x y z -> [x, y, z]) xs ys zs
  where ranks = board

searchX :: Board Char -> Int
searchX = count isXMAS . all3x3
  where
    isXMAS :: [[Char]] -> Bool
    isXMAS = \case
      [ ['M', _, 'M'], [_, 'A', _], ['S', _, 'S'] ] -> True
      [ ['M', _, 'S'], [_, 'A', _], ['M', _, 'S'] ] -> True
      [ ['S', _, 'M'], [_, 'A', _], ['S', _, 'M'] ] -> True
      [ ['S', _, 'S'], [_, 'A', _], ['M', _, 'M'] ] -> True
      _ -> False
