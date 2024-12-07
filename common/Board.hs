{-# LANGUAGE UndecidableInstances #-}
-- | Useful for 2D puzzles ig
module Board
  (
  -- * Board type
    Board
  -- * Re-exports
  , module Control.Lens
  , module Linear.V2
  , module IsList
  , module Control.Zipper
  ) where

import Linear.V2
import Control.Lens
import Text.Show.Pretty
import GHC.IsList as IsList
import Data.List (intercalate)
import Control.Zipper
import Data.HashMap.Lazy as Map
import Data.Hashable

-- | A 2D board
newtype Board a = Board (HashMap (V2 Int) a)
  deriving newtype (Eq, Hashable)

instance (Show a) => Show (Board a) where
  show = unlines . fmap (intercalate "\t" . fmap show) . IsList.toList

type instance IxValue (Board a) = a
type instance Index (Board a) = V2 Int

instance Ixed (Board a)

instance At (Board a) where
  at :: V2 Int -> Lens' (Board a) (Maybe a)
  at coords = lens ((!? coords) . \(Board m) -> m) setter
    where setter = \cases
            (Board m) Nothing -> Board $ Map.delete coords m
            (Board m) (Just a) -> Board $ Map.adjust (const a) coords m


-- | Use to construct/deconstruct a board
instance IsList (Board a) where
  type Item (Board a) = [a]
  fromList :: [[a]] -> Board a
  fromList rows = Board $ Map.fromList
    [(V2 x y, val) | (y, row) <- zip [0..] rows, (x, val) <- zip [0..] row]
  toList :: Board a -> [[a]]
  toList (Board m) = [ [m ! V2 x y | x <- [0..maxX]] | y <- [0..maxY] ]
    where myKeys = Map.keys m
          maxX = maximum [x | V2 x _ <- myKeys]
          maxY = maximum [y | V2 _ y <- myKeys]


