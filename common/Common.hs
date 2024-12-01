module Common
  ( module Data.Function
  , input
  ) where

import Control.Monad.IO.Class
import Data.String
import Paths_aoc
import Data.Function

input :: (IsString str, MonadIO m) => m str
input = liftIO $ do
  path <- getDataFileName "input.txt"
  contents <- readFile path
  pure (fromString contents)
