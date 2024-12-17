module Board
  ( Board
  -- * From/to 2D lists
  , fromMatrix
  , fromMatrixWith
  , toMatrixWith
  -- * Pretty print
  , render
  -- * Re-exports
  , module Linear.V2
  , HashMap
  , (!?)
  , Hashable
  ) where

import Linear.V2
import Data.HashMap.Lazy
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable (Hashable)

-- | A board is a @HashMap@ from 2D coordinates (@V2 idx@) to values
type Board idx a = HashMap (V2 idx) a

fromMatrix :: (Hashable idx, Num idx, Enum idx) => [[a]] -> Board idx a
fromMatrix = fromMatrixWith Just

fromMatrixWith :: (Hashable idx, Num idx, Enum idx)
               => (a -> Maybe b)  -- ^ What the element will eventually be.
                                  -- Return @Nothing@ to discard.
               -> [[a]]
               -> Board idx b
fromMatrixWith f matrix = HashMap.fromList $ do
  (y, rows) <- zip [0..] matrix
  (x, ch) <- zip [0..] rows
  case f ch of
    Just b  -> pure (V2 x y, b)
    Nothing -> []

-- | Dual of @fromMatrixWith@
toMatrixWith :: (Hashable idx, Num idx, Enum idx)
             => (Maybe a -> b)  -- ^ Transform
             -> idx             -- ^ height (number of rows)
             -> idx             -- ^ width (number of cols)
             -> Board idx a
             -> [[b]]
toMatrixWith f h w board = do
    y <- [0 .. h - 1]
    pure $ do
      x <- [0 .. w - 1]
      pure . f $ board !? V2 x y

-- | Pretty show board, AOC style - each square be a char
render :: (Hashable idx, Num idx, Enum idx)
       => (Maybe a -> Char)
       -> idx  -- ^ height
       -> idx  -- ^ width
       -> Board idx a
       -> String
render f h w = unlines . toMatrixWith f h w
