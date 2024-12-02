module Common
  ( module Data.Function
  , module Control.Monad
  , module Control.Applicative
  , module Control.Lens
  , module Control.Monad.IO.Class
  , module Data.Foldable
  , input
  , counts
  , count
  ) where

import Control.Monad.IO.Class
import Data.String
import Paths_aoc
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (genericLength)
import Control.Monad
import Control.Applicative
import Control.Lens
import Data.Foldable

input :: (IsString str, MonadIO m) => m str
input = liftIO $ do
  path <- getDataFileName "input.txt"
  contents <- readFile path
  pure (fromString contents)

counts :: (Num n, Ord a, Foldable t) => t a -> Map a n
counts = foldr (\x -> Map.insertWith (+) x 1) mempty

count :: (Num n) => (a -> Bool) -> [a] -> n
count p = genericLength . mfilter p
