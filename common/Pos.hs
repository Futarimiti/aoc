{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DataKinds #-}

module Pos
  (
  -- * Position types
    Pos2 (..)
  , Pos3 (..)
  -- * Classy lenses
  , HasX (..)
  , HasY (..)
  , HasZ (..)
  -- * Stateful actions
  , forward
  , backward
  , upward
  , downward
  , inward
  , outward
  -- * Convenient re-exports
  , module Control.Lens
  ) where

import Common
import MTL
import Control.Lens
import Data.Kind

class HasX s a | s -> a where
  x :: Lens' s a

class HasY s a | s -> a where
  y :: Lens' s a

class HasZ s a | s -> a where
  z :: Lens' s a

-- | 2D position
newtype Pos2 a = Pos2 { getPos2 :: (a, a) }
  deriving newtype (Show, Eq, Read)

instance Field1 (Pos2 n) (Pos2 n) n n where
  _1 = lens (\(Pos2 (xVal, _)) -> xVal) (\(Pos2 (_, yVal)) newX -> Pos2 (newX, yVal))

instance Field2 (Pos2 n) (Pos2 n) n n where
  _2 = lens (\(Pos2 (_, yVal)) -> yVal) (\(Pos2 (xVal, _)) newY -> Pos2 (xVal, newY))

instance HasX (Pos2 a) a where
  x = lens (\(Pos2 (xVal, _)) -> xVal) (\(Pos2 (_, yVal)) newX -> Pos2 (newX, yVal))

instance HasY (Pos2 a) a where
  y = lens (\(Pos2 (_, yVal)) -> yVal) (\(Pos2 (xVal, _)) newY -> Pos2 (xVal, newY))


-- | 3D position
newtype Pos3 a = Pos3 { getPos3 :: (a, a, a) }
  deriving newtype (Show, Eq, Read)

instance Field1 (Pos3 n) (Pos3 n) n n where
  _1 = lens (\(Pos3 (xVal, _, _)) -> xVal) (\(Pos3 (_, yVal, zVal)) newX -> Pos3 (newX, yVal, zVal))

instance Field2 (Pos3 n) (Pos3 n) n n where
  _2 = lens (\(Pos3 (_, yVal, _)) -> yVal) (\(Pos3 (xVal, _, zVal)) newY -> Pos3 (xVal, newY, zVal))

instance Field3 (Pos3 n) (Pos3 n) n n where
  _3 = lens (\(Pos3 (_, _, zVal)) -> zVal) (\(Pos3 (xVal, yVal, _)) newZ -> Pos3 (xVal, yVal, newZ))

instance HasX (Pos3 a) a where
  x = lens (\(Pos3 (xVal, _, _)) -> xVal) (\(Pos3 (_, yVal, zVal)) newX -> Pos3 (newX, yVal, zVal))

instance HasY (Pos3 a) a where
  y = lens (\(Pos3 (_, yVal, _)) -> yVal) (\(Pos3 (xVal, _, zVal)) newY -> Pos3 (xVal, newY, zVal))

instance HasZ (Pos3 a) a where
  z = lens (\(Pos3 (_, _, zVal)) -> zVal) (\(Pos3 (xVal, yVal, _)) newZ -> Pos3 (xVal, yVal, newZ))

-- | X-axis movement
forward, backward :: (MonadState s m, Num n, HasX s n) => n -> m ()
forward = (x +=)
backward = (x -=)

-- | Y-axis movement
upward, downward :: (MonadState s m, Num n, HasY s n) => n -> m ()
upward = (y +=)
downward = (y -=)

-- | Z-axis movement
inward, outward :: (MonadState s m, Num n, HasZ s n) => n -> m ()
inward = (z -=)
outward = (z +=)
