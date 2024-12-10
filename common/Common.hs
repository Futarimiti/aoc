{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Common
  ( input
  , counts
  , count
  -- * Re-exports
  , module Data.Function
  , module Control.Monad
  , module Control.Applicative
  , module Data.Functor
  , module Control.Lens
  , module Control.Monad.IO.Class
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Traversable
  , module Witherable
  , andM
  , orM
  , allM
  , anyM
  , every
  , i
  , mid
  ) where

import Data.Functor
import Control.Monad.IO.Class
import Data.String
import Paths_aoc
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (group, sort, tails)
import Control.Monad
import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Ord
import Data.Maybe hiding (catMaybes, mapMaybe)
import Witherable hiding (filter)
import Data.String.Interpolate
import Data.Traversable

-- -- | Mode value, in the statistical sense
-- mode :: Ord a => [a] -> a
-- mode = head . maximumBy (comparing length) . group . sort

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

anyM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
anyM f = foldr (orM . f) (pure False)

allM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
allM f = foldr (andM . f) (pure True)

-- | Monadic version of or
orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

-- | Monadic version of and
andM :: Monad m => m Bool -> m Bool -> m Bool
andM m1 m2 = m1 >>= \x -> if x then m2 else return False

-- | Every @n@ consecutive elements
every :: Int -> [a] -> [[a]]
every n xs = mapMaybe (take' n) (tails xs)
  where take' :: Int -> [a] -> Maybe [a]
        take' 0 _      = Just []
        take' _ []     = Nothing
        take' n (x:xs) = (x:) <$> take' (n - 1) xs

-- | Find the element in the middle of a list.
-- If the list contains even number of elements, return @Nothing@.
-- Hangs for infinite list
mid :: [a] -> Maybe a
mid = join go
  where go _ []            = Nothing
        go (x:_) [_]       = Just x
        go (_:xs) (_:_:ys) = go xs ys
        go _ _             = Nothing
