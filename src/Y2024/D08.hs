module Y2024.D08 () where

import Common
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import MTL
import Data.Hashable
import qualified L
import NE (NonEmpty)
import qualified NE
import Linear.V2

-- Ideally @Set (V2 Int)@ would be generally preferrable,
-- unfortunately no one could give a good implementation of lazy set
-- (there will be infinite elements)
-- remember to nub

type Antinodes = HashMap Char [V2 Int]
type Antennae = HashMap Char [V2 Int]

-- >>> emit (V2 0 0) (V2 1 2)
-- [V2 2 4,V2 3 6,V2 4 8...
emit :: V2 Int             -- | point source
     -> V2 Int             -- | first incidence
     -> NonEmpty (V2 Int)  -- | propagate infinitely
emit p1 p2 = NE.fromList $ drop 1 $ iterate (+ (p2 - p1)) p2  -- safe

interleave :: [a] -> [a] -> [a]
interleave [] ys         = ys
interleave xs []         = xs
interleave (x:xs) (y:ys) = x:y:interleave xs ys

-- | Given coords of two antennae, find the positions of exactly 2 arising antinodes.
-- (part1)
antinodes1 :: Int -> Int -> V2 Int -> V2 Int -> [V2 Int]
antinodes1 cols rows p1 p2 = filter inBound [p1side, p2side]
  where p1side = NE.head $ emit p2 p1
        p2side = NE.head $ emit p1 p2
        inBound (V2 x y) = and @[] [x >= 0, x < cols, y >= 0, y < rows]

-- | Given coords of two antennae, find the positions of all infinite arising antinodes.
-- (part2)
antinodes2 :: Int -> Int -> V2 Int -> V2 Int -> [V2 Int]
antinodes2 cols rows p1 p2 = interleave (NE.takeWhile inBound p1side) (NE.takeWhile inBound p2side)
  where p1side = p1 `NE.cons` emit p2 p1
        p2side = p2 `NE.cons` emit p1 p2
        inBound (V2 x y) = and @[] [x >= 0, x < cols, y >= 0, y < rows]

getAntinodes :: forall m. MonadReader Antennae m
             => (V2 Int -> V2 Int -> [V2 Int])  -- antinodes strategy
             -> m Antinodes
getAntinodes strat = Map.map (uncurry strat <=< pairs) <$> ask
  where pairs ps = [(x, y) | (x:ys) <- L.tails ps, y <- ys]  -- unique pairs

instance AOC 2024 8 where
  -- antennae distribution + graph col & row count
  type Input 2024 8 = (Antennae, Int, Int)
  parse :: [[Char]] -> Input 2024 8
  parse css = (antennae, cols, rows)
    where
      antennae :: Antennae
      antennae = Map.fromListWith L.union $ do
        (y, row) <- zip [0..] css
        (x, c) <- zip [0..] row
        guard $ c /= '.'
        pure (c, [V2 x y])
      rows = length css
      cols = case css of
        (h:_) -> length h
        _     -> error "wtf?"

  type Output1 2024 8 = Int
  part1 :: Input 2024 8 -> Output1 2024 8
  part1 (antennae, cols, rows) = length antinodes
    where antinodes = getAntinodes (antinodes1 cols rows) antennae & Map.elems & foldr1 L.union

  type Output2 2024 8 = Int
  part2 :: Input 2024 8 -> Output2 2024 8
  part2 (antennae, cols, rows) = length antinodes
    where antinodes = getAntinodes (antinodes2 cols rows) antennae & Map.map (L.filter inBound) & Map.elems & foldr1 L.union & L.nub
          inBound (V2 x y) = and @[] [x >= 0, x < cols, y >= 0, y < rows]

