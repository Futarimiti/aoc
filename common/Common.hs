module Common
  ( input
  , counts
  , count
  -- * Re-exports
  , module Data.Function
  , module Control.Monad
  , module Control.Applicative
  , module Control.Lens
  , module Control.Monad.IO.Class
  , module Data.Foldable
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

-- | Gets the AoC input file.
input :: (IsString str, MonadIO m) => m str
input = liftIO $ do
  path <- getDataFileName "input.txt"
  contents <- readFile path
  pure (fromString contents)

-- | Count occurrences of elements within a @'Foldable'@,
-- summarising in a @Map@.
counts :: (Num n, Ord a, Foldable t) => t a -> Map a n
counts = foldr (\x -> Map.insertWith (+) x 1) mempty

-- | Count occurrences of elements within a @'Foldable'@ and @'MonadPlus'@.
count :: (Num n, MonadPlus t, Foldable t) => (a -> Bool) -> t a -> n
count p = fromIntegral . length . mfilter p
