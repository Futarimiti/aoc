{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Y2024.D09 () where

import Common
import Numeric.Natural
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified L
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Trace
import Data.Bifunctor (second)
import Debug.Trace.Dbg (dbg)
import Data.Char (digitToInt)

type Size = Natural
type ID = Natural
data Block = File ID | Free deriving (Show, Eq)
data FSStatus = Sparse | Compact

-- Seq because it allows insertAt
type family FileSystem s where
  FileSystem Sparse = Seq (Block, Size)
  FileSystem Compact = Seq (ID, Size)

moveLeftFrags :: FileSystem Sparse -> FileSystem Compact
moveLeftFrags sys = case uncons sys of
  Nothing                         -> empty
  Just ((File id', size), remain) -> (id', size) <| moveLeftFrags remain
  Just ((Free, size), remain)     -> moveLeftFrags $ case unsnoc remain of
    Nothing                       -> empty
    Just (init', (Free, _))       -> (Free, size) <| init'
    Just (init', (File id', len)) -> case compare len size of
      LT -> (File id', len) <| (Free, size - len) <| init'
      GT -> (File id', size) <| (init' |> (File id', len - size))
      EQ -> (File id', size) <| init'

moveLeftWhole :: FileSystem Sparse -> FileSystem Sparse
moveLeftWhole sys = case uncons sys of
  Nothing                        -> empty
  Just ((File id', size), tail') -> (File id', size) <| moveLeftWhole tail'
  Just ((Free, size), tail')     -> case unsnoc tail' of
    Nothing                     -> empty
    Just (mid, free@(Free, _))  -> moveLeftWhole ((Free, size) <| mid) |> free
    Just (mid, (File id', len)) -> case tryFill (id', len) ((Free, size) <| mid) of
      Nothing   -> moveLeftWhole ((Free, size) <| mid) |> (File id', len)
      Just sys' -> moveLeftWhole sys' |> (Free, len)

-- Try fill a whole block of file into a sparse file system,
-- from left to rightmost
-- On success, return the resulting file system
tryFill :: (ID, Size) -> FileSystem Sparse -> Maybe (FileSystem Sparse)
tryFill (id', size) sys = do
  idx <- Seq.findIndexL bigEnough sys
  let
    reduceFree = Seq.adjust (second $ subtract size) idx
    insertFile = Seq.insertAt idx (File id', size)
  pure (sys & reduceFree & insertFile)
  where
    bigEnough (Free, size') | size' >= size = True
    bigEnough _ = False

-- part 1
checksum :: FileSystem Compact -> Natural
checksum = go 0
  where
    go :: Natural -> FileSystem Compact -> Natural
    go pos sys = case uncons sys of
      Nothing -> 0
      Just ((id', size), remain) ->
        sum (L.genericTake size [id' * p | p <- [pos..]]) + go (pos + size) remain

-- part 2
checksum' :: FileSystem Sparse -> Natural
checksum' = go 0
  where
    go :: Natural -> FileSystem Sparse -> Natural
    go pos sys = case uncons sys of
      Nothing -> 0
      Just ((Free, size), remain) -> go (pos + size) remain
      Just ((File id', size), remain) ->
        sum (L.genericTake size [id' * p | p <- [pos..]]) + go (pos + size) remain

instance AOC 2024 9 where
  type Input 2024 9 = FileSystem Sparse
  parse :: [String] -> Input 2024 9
  parse [ln] = fromDiskMap $ fmap (fromIntegral . digitToInt) ln
    where
      fromDiskMap :: [Natural] -> FileSystem Sparse
      fromDiskMap = go 0
        where
          go _ []         = []
          go id' [x]      = [(File id', x)]
          go id' (x:y:xs) = [(File id', x), (Free, y)] <> go (id' + 1) xs
  parse _ = error "impossible"

  type Output1 2024 9 = Natural
  part1 :: Input 2024 9 -> Output1 2024 9
  part1 = checksum . moveLeftFrags

  type Output2 2024 9 = Natural
  part2 :: Input 2024 9 -> Output2 2024 9
  part2 = checksum' . moveLeftWhole

