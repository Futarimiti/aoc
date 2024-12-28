module Y2024.D18 () where

import Common
import Board
import Data.List.Split (splitOn)
import Data.Set (Set)
import MTL
import qualified Data.Set as Set
import Algo
import qualified ListT
import qualified L

instance AOC 2024 18 where
  type Input 2024 18 = [Pos]
  parse :: [String] -> Input 2024 18
  parse = fmap $ \line -> case splitOn "," line of
    [str1, str2] -> V2 (read str1) (read str2)
    _            -> error "malformed input"

  type Output1 2024 18 = Integer
  part1 :: Input 2024 18 -> Output1 2024 18
  part1 bytes = flip evalState Set.empty $ do
    lands $ take 1024 bytes
    mroute <- shortestPath
    case mroute of
      Nothing    -> error "cannot find a path"
      Just route -> pure $ L.genericLength route

  type Output2 2024 18 = Pos
  part2 :: Input 2024 18 -> Output2 2024 18
  part2 bytes' = flip evalState Set.empty $ do
    let (bs, remain) = splitAt 1810 bytes'
    lands bs
    go remain
    where
      go :: (MonadState (Set Pos) m) => [Pos] -> m Pos
      go [] = error "never blocking"
      go (b:bs) = do
        land b
        mroute <- findPath
        case mroute of
          Nothing -> pure b
          Just path -> do
            let (irrelevant, blocking) = span (`notElem` path) bs
            lands irrelevant
            go blocking

type Pos = V2 Int

-- | Land a byte at specific pos
land :: MonadState (Set Pos) m => Pos -> m ()
land = modify . Set.insert

-- | Land multiple bytes
lands :: (MonadState (Set Pos) m, Foldable f) => f Pos -> m ()
lands = traverse_ land

-- | Try to find shortest path from upper left @(0,0)@ to bottom right @(70,70)@
shortestPath :: forall m. (MonadState (Set Pos) m) => m (Maybe [Pos])
shortestPath = do
  res <- dijkstraM next expense arrived (V2 0 0)
  pure $ fmap snd res
  where
    next :: Pos -> m [Pos]
    next pos = ListT.toList $ do
      step <- ListT.fromFoldable @m @[] [_x +~ 1, _x -~ 1, _y +~ 1, _y -~ 1]
      let neighbour = step pos
      free <- gets (neighbour `notElem`)
      guard $ free && inBound neighbour
      pure neighbour
    inBound (V2 x y) = x >= 0 && x <= 70 && y >= 0 && y <= 70
    arrived :: Pos -> m Bool
    arrived = pure . (== V2 70 70)
    expense :: Pos -> Pos -> m Nat
    expense _ _ = pure 1

-- | Try to find any path from upper left @(0,0)@ to bottom right @(70,70)@
findPath :: forall m. (MonadState (Set Pos) m) => m (Maybe [Pos])
findPath = dfsM next arrived (V2 0 0)
  where
    next :: Pos -> m [Pos]
    next pos = ListT.toList $ do
      step <- ListT.fromFoldable @m @[] [_x +~ 1, _x -~ 1, _y +~ 1, _y -~ 1]
      let neighbour = step pos
      free <- gets (neighbour `notElem`)
      guard $ free && inBound neighbour
      pure neighbour
    inBound (V2 x y) = x >= 0 && x <= 70 && y >= 0 && y <= 70
    arrived :: Pos -> m Bool
    arrived = pure . (== V2 70 70)
