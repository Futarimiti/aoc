module Y2024.D06.Part1 (part1) where

import Common
import Y2024.D06.Common
import MTL
import Board
import Data.HashSet as Set

part1 :: IO ()
part1 = do
  contents <- input
  let board = Board.fromList $ lines contents
      start = Guard (startPos board) U
      guards = flip execAccum Set.empty $ flip runReaderT board $ flip runStateT start $ runExceptT walk
  let visits = Set.map (view pos) guards
  print (length visits)
