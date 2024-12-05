-- | @import qualified L@
module L
  ( module Data.List
  , sortByM
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
