module Y2024.D06.Part2 (part2) where

import Common
import Y2024.D06.Common
import MTL
import Board
import Data.HashSet as Set
import Debug.Trace

-- add obstacle: board & (V2 x y) .~ '#'

type Set = Set.HashSet

-- Took ~1hr to finish running, got correct answer anyway
-- All's Well That Ends Well ig
part2 :: IO ()
part2 = do
  contents <- input
  let board = Board.fromList $ lines contents
      start = Guard (startPos board) U
      guards = flip execAccum Set.empty $ flip runReaderT board $ flip runStateT start $ runExceptT walk
  traceM "got guards"
  let visits :: Set (V2 Int)
      visits = Set.map (view pos) guards
      boardVariants :: Set (Board Char)
      boardVariants = Set.map (\pos' -> board & ix pos' .~ '#') visits
  traceM "got variants"
  let loopedVariants :: Set (Board Char)
      loopedVariants = Set.filter endsInLoop boardVariants
      endsInLoop :: Board Char -> Bool
      endsInLoop b = trace "checking a board" $
        Left InLoop == evalAccum (flip runReaderT b $ flip evalStateT start $ runExceptT walk) Set.empty
  print (length loopedVariants)
