{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}

-- | @import qualified L@
module L
  ( module Data.List
  , sortByM
  , mid
  , every
  , isPalindrome
  ) where

import Data.List
import Common

sortByM :: (Monad m) => (a -> a -> m Ordering) -> [a] -> m [a]
sortByM cmp = mergeAll <=< sequences
  where
    sequences (a:b:xs) = do
      ord <- cmp a b
      case ord of
        GT -> descending b [a] xs
        _  -> ascending b (a:) xs
    sequences xs = return [xs]

    descending a as cs@(b:bs) = do
      ord <- cmp a b
      case ord of
        GT -> descending b (a:as) bs
        _  -> ((a:as) :) <$> sequences cs
    descending a as bs = ((a:as) :) <$> sequences bs

    ascending a as cs@(b:bs) = do
      ord <- cmp a b
      case ord of
        GT -> (as [a] :) <$> sequences cs
        _  -> ascending b (\ys -> as (a:ys)) bs
    ascending a as bs = (as [a] :) <$> sequences bs

    mergeAll [x] = return x
    mergeAll xs  = mergeAll =<< mergePairs xs

    mergePairs (a:b:xs) = liftM2 (:) (merge a b) $ mergePairs xs
    mergePairs xs       = return xs

    merge as@(a:as') bs@(b:bs') = do
      ord <- cmp a b
      case ord of
        GT -> (b :) <$> merge as  bs'
        _  -> (a :) <$> merge as' bs
    merge [] bs = return bs
    merge as [] = return as

-- | Find the element in the middle of a list.
-- If the list contains even number of elements, return @Nothing@.
-- Hangs for infinite list
mid :: [a] -> Maybe a
mid = join go
  where go _ []            = Nothing
        go (x:_) [_]       = Just x
        go (_:xs) (_:_:ys) = go xs ys
        go _ _             = Nothing

-- | Every @n@ consecutive elements
--
-- Maybe use @extend@ some day?
every :: Int -> [a] -> [[a]]
every n xs = take' n <$?> tails xs
  where take' :: Int -> [a] -> Maybe [a]
        take' 0 _        = Just []
        take' _ []       = Nothing
        take' n' (x:xs') = (x:) <$> take' (n' - 1) xs'

isPalindrome :: Eq a => [a] -> Bool
isPalindrome str = step str 0
  where step xs idx
          | idx >= (length xs `div` 2) = True
          | xs !! idx /= xs !! (length xs - idx - 1) = False
          | otherwise = step xs (idx + 1)
