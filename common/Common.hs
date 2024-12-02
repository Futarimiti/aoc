module Common
  ( module Data.Function
  , input
  , counts
  ) where

import Control.Monad.IO.Class
import Data.String
import Paths_aoc
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

input :: (IsString str, MonadIO m) => m str
input = liftIO $ do
  path <- getDataFileName "input.txt"
  contents <- readFile path
  pure (fromString contents)

counts :: (Num n, Ord a, Foldable t) => t a -> Map a n
counts = foldr (\x -> Map.insertWith (+) x 1) mempty
