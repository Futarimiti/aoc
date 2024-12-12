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
import Debug.Trace.Dbg
import MTL

-- | Consecutive block of plants
--
-- Region identity not matter in part 1, might be useful in part 2
data Region = Region
  { _name   :: Char
  , _coords :: (HashSet (V2 Int))
  } deriving (Show, Eq, Ord)

makeLenses ''Region

instance AOC 2024 12 where
  type Input 2024 12 = HashMap (V2 Int) Char
  parse :: [String] -> Input 2024 12
  parse matrix = HashMap.fromList $ do
    (y, row) <- zip [0..] matrix
    (x, c) <- zip [0..] row
    pure (V2 x y, c)

  type Output1 2024 12 = Nat
  part1 :: Input 2024 12 -> Output1 2024 12
  part1 = sum . MultiSet.map price . aggregate

  type Output2 2024 12 = Nat
  part2 :: Input 2024 12 -> Output2 2024 12
  part2 = sum . MultiSet.map price' . aggregate

aggregate :: HashMap (V2 Int) Char -> MultiSet Region
aggregate = trace "aggregate" $ MultiSet.fromList . L.unfoldr \board -> do
  (start, label) <- lookupMin board
  [dbgM|label|]
  let
    step :: V2 Int -> [V2 Int]
    step i' = do
      neighbour <- neighbouringCoords i'
      guard $ board !? neighbour == Just label
      pure neighbour
    myCoords = floodFill step start
  [dbgM|myCoords|]
  pure (Region label myCoords, HashMap.filterWithKey (\k _ -> k `notElem` myCoords) board)
  where
    lookupMin :: Ord k => HashMap k v -> Maybe (k, v)
    lookupMin m
      | HashMap.null m = Nothing
      | otherwise = pure . minimumBy (comparing fst) . HashMap.toList $ m
    neighbouringCoords :: V2 Int -> [V2 Int]
    neighbouringCoords = (<$> [_x +~ 1, _x -~ 1, _y +~ 1, _y -~ 1]) . (&)

area :: Region -> Nat
area = traceShowId . trace "area" . fromIntegral . length . view coords

perimeter :: forall m. MonadReader Region m => m Nat
perimeter = do
  myCoords <- view coords
  exposed <- traverse exposedSides (HashSet.toList myCoords)  -- HashSet not traversable
  pure $ sum exposed
  where
    exposedSides :: V2 Int -> m Nat
    exposedSides coord = do
      let neighbouringCoords = ($ coord) <$> [_x +~ 1, _x -~ 1, _y +~ 1, _y -~ 1]
      neighbours <- filterA ((<$> view coords) . elem) neighbouringCoords
      pure $ 4 - L.genericLength neighbours

price :: Region -> Nat
price region = [dbg|area region|] * [dbg|perimeter region|]

price' :: Region -> Nat
price' region = [dbg|area region|] * [dbg|walls region|]

walls :: Region -> Nat
walls = corners

corners :: Region -> Nat
corners = undefined

