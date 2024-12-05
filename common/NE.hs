-- | @import qualified NE@
module NE
  ( module Data.List.NonEmpty
  , sortByM
  ) where

import Data.List.NonEmpty
import Common
import qualified L
import GHC.IsList as IsList

sortByM :: (Monad m) => (a -> a -> m Ordering) -> NonEmpty a -> m (NonEmpty a)
sortByM cmp xs = IsList.fromList <$> L.sortByM cmp (IsList.toList xs)
