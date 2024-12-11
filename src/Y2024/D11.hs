module Y2024.D11 () where

import Common
import Memoize
import qualified L
import MTL
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map

instance AOC 2024 11 where
  type Input _ _ = [Integer]
  parse = fmap read . words . head
  part1 = evolve 25
  part2 = evolve 75

type Stone = Integer

blink :: Stone -> [Stone]
blink = \case
  0 -> [1]
  x | str <- show x, len <- length str, even len
    , (leftHalf, rightHalf) <- L.splitAt (len `div` 2) str
    -> [read leftHalf, read rightHalf]
  x -> [x * 2024]

evolve1 :: (Memoized (Integer, Stone) Integer m)
        => Integer    -- iteration #
        -> Stone
        -> m Integer  -- stone # after n blinks
evolve1 times stones = (times, stones) & retrieveOrM case times of
  0 -> pure 1
  _ -> do
    let line = blink stones
    further <- traverse (evolve1 (times - 1)) line
    pure $ sum further

evolve :: Integer -> [Stone] -> Integer
evolve times = sum . evalMemoized . traverse (evolve1 times)
