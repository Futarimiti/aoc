module Y2024.D08 where

import Common
import Board
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import MTL
import Data.Hashable
import qualified L

-- Ideally @Set (V2 Int)@ would be generally preferrable,
-- unfortunately no one could give a good implementation of lazy set
-- (there will be infinite elements)
-- remember to nub

type Antinodes = HashMap Char [V2 Int]
type Antennae = HashMap Char [V2 Int]

-- NOTE '.' is not an antenna
readAntennae :: [String] -> (Antennae, Int, Int)
readAntennae css = (antennae, cols, rows)
  where
    rows = length css
    cols = length (head css)
    antennae = Map.fromListWith L.union $ do
      (pos, char) <- [(V2 x y, css !! y !! x) | x <- [0..cols-1], y <- [0..rows-1]]
      guard (char /= '.')
      pure (char, [pos])

-- >>> emit (V2 0 0) (V2 1 2)
-- [V2 2 4,V2 3 6,V2 4 8...
emit :: V2 Int    -- | point source
     -> V2 Int    -- | first incidence
     -> [V2 Int]  -- | propagate infinitely
emit p1 p2 = drop 1 $ iterate (+ (p2 - p1)) p2

interleave :: [a] -> [a] -> [a]
interleave [] ys         = ys
interleave xs []         = xs
interleave (x:xs) (y:ys) = x:y:interleave xs ys

-- | Given coords of two antennae, find the positions of exactly 2 arising antinodes.
-- (part1)
antinodes1 :: Int -> Int -> V2 Int -> V2 Int -> [V2 Int]
antinodes1 cols rows p1 p2 = filter inBound [p1side, p2side]
  where p1side = head $ emit p2 p1
        p2side = head $ emit p1 p2
        inBound (V2 x y) = and @[] [x >= 0, x < cols, y >= 0, y < rows]

-- | Given coords of two antennae, find the positions of all infinite arising antinodes.
-- (part2)
antinodes2 :: Int -> Int -> V2 Int -> V2 Int -> [V2 Int]
antinodes2 cols rows p1 p2 = interleave (L.takeWhile inBound p1side) (L.takeWhile inBound p2side)
  where p1side = p1 : emit p2 p1
        p2side = p2 : emit p1 p2
        inBound (V2 x y) = and @[] [x >= 0, x < cols, y >= 0, y < rows]

getAntinodes :: forall m. MonadReader Antennae m
             => (V2 Int -> V2 Int -> [V2 Int])  -- antinodes strategy
             -> m Antinodes
getAntinodes strat = Map.map (uncurry strat <=< pairs) <$> ask
  where pairs ps = [(x, y) | (x:ys) <- L.tails ps, y <- ys]  -- unique pairs

part1 :: IO ()
part1 = do
  (lines -> contents) <- input @String
  let (antennae, cols, rows) = readAntennae contents
      antinodes = getAntinodes (antinodes1 cols rows) antennae & Map.elems & foldr1 L.union
  antinodes & length & print

part2 :: IO ()
part2 = do
  (lines -> contents) <- input @String
  let (antennae, cols, rows) = readAntennae contents
      inBound (V2 x y) = and @[] [x >= 0, x < cols, y >= 0, y < rows]
      antinodes = getAntinodes (antinodes2 cols rows) antennae & Map.map (L.filter inBound) & Map.elems & foldr1 L.union & L.nub
  antinodes & length & print
