{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2021.D02 (execLine, execLine') where

import Common
import MTL
import Pos

-- part1
execLine :: (MonadState (Pos2 Int) m) => String -> m ()
execLine (words -> [direction, read -> steps]) = move steps
  where move = case direction of
                 "forward" -> forward
                 "up"      -> upward
                 "down"    -> downward

-- part2

data Submarine = Submarine
  { _pos :: Pos2 Int
  , _aim :: Int
  } deriving (Show)

makeLenses ''Submarine

instance HasX Submarine Int where
  x = pos . x

instance HasY Submarine Int where
  y = pos . y

execLine' :: (MonadState Submarine m) => String -> m ()
execLine' (words -> [direction, read -> steps]) = case direction of
  "forward" -> do
    currAim <- gets (view aim)
    forward steps
    downward $ steps * currAim
  "up"      -> aim -= steps
  "down"    -> aim += steps
