{-# LANGUAGE BangPatterns #-}
module Y2024.D12 () where

import Common
import Linear.V2
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HashMap
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import qualified L
import Algo
import MTL
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

instance AOC 2024 12 where
  type Input 2024 12 = HashMap (V2 Int) Char
  parse :: [String] -> Input 2024 12
  parse matrix = HashMap.fromList $ do
    (y, row) <- zip [0..] matrix
    (x, c) <- zip [0..] row
    pure (V2 x y, c)

  type Output1 2024 12 = Nat
  part1 :: Input 2024 12 -> Output1 2024 12
  part1 = sum . MultiSet.map (liftA2 (*) area perimeter) . aggregate

  type Output2 2024 12 = Nat
  part2 :: Input 2024 12 -> Output2 2024 12
  part2 = sum . MultiSet.map (liftA2 (*) area walls) . aggregate

-- | Consecutive block of plants
type Region = HashSet (V2 Int)

aggregate :: HashMap (V2 Int) Char -> MultiSet Region
aggregate = MultiSet.fromList . L.unfoldr \board -> do
  (start, label) <- lookupMin board
  let
    step :: V2 Int -> [V2 Int]
    step i' = do
      neighbour <- neighbouringCoords i'
      guard $ board !? neighbour == Just label
      pure neighbour
    myCoords = floodFill step start
  pure (myCoords, HashMap.filterWithKey (\k _ -> k `notElem` myCoords) board)
  where
    lookupMin :: Ord k => HashMap k v -> Maybe (k, v)
    lookupMin m
      | null m = Nothing
      | otherwise = pure . minimumBy (comparing fst) . HashMap.toList $ m
    neighbouringCoords :: V2 Int -> [V2 Int]
    neighbouringCoords = (<$> [l,r,u,d]) . (&)

area :: Region -> Nat
area = fromIntegral . length

perimeter :: forall m. MonadReader Region m => m Nat
perimeter = do
  myCoords <- ask
  exposed <- traverse exposedSides (HashSet.toList myCoords)  -- HashSet not traversable
  pure $ sum exposed
  where
    exposedSides :: V2 Int -> m Nat
    exposedSides coord = do
      let neighbouringCoords = ($ coord) <$> [l,r,u,d]
      neighbours <- filterA ((<$> ask) . elem) neighbouringCoords
      pure $ 4 - L.genericLength neighbours

walls :: forall m. MonadReader Region m => m Nat
walls = do
  (HashSet.toList -> myCoords) <- ask
  -- | expect overlaps
  ul <- countA (corner l u) myCoords
  ur <- countA (corner u r) myCoords
  dl <- countA (corner d l) myCoords
  dr <- countA (corner r d) myCoords
  pure $ sum @[] [ul, ur, dl, dr]
  where
    corner :: (V2 Int -> V2 Int) -> (V2 Int -> V2 Int) -> V2 Int -> m Bool
    corner dir1 dir2 x = do
      myCoords <- ask
      let open dir = dir x `notElem` myCoords
      pure $ open dir1 && (open dir2 || not (open (dir1 . dir2)))

-- movements
l, r, u, d :: V2 Int -> V2 Int
l = _x -~ 1
r = _x +~ 1
u = _y -~ 1
d = _y +~ 1
