{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Y2021.D02 (execLine, execLine') where

import Common
import MTL
import Linear.V2

forward, upward, downward :: (R2 v2, MonadState (v2 n) m, Num n) => n -> m ()
forward = (_x +=)
upward = (_y +=)
downward = (_y -=)

-- part1
execLine :: (MonadState (V2 Int) m) => String -> m ()
execLine (words -> [direction, read -> steps]) = move steps
  where move = case direction of
                 "forward" -> forward
                 "up"      -> upward
                 "down"    -> downward

-- part2

data Submarine a = Submarine
  { _pos :: V2 a
  , _aim :: a
  } deriving (Show)

makeLenses ''Submarine

instance R1 Submarine where
  _x = pos . _x

instance R2 Submarine where
  _xy = pos . _xy

execLine' :: (MonadState (Submarine n) m, Num n, Read n) => String -> m ()
execLine' (words -> [direction, read -> steps]) = case direction of
  "forward" -> do
    currAim <- use aim
    forward steps
    downward $ steps * currAim
  "up"      -> aim -= steps
  "down"    -> aim += steps
