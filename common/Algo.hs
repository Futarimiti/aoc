module Algo
  ( floodFill
  , module Algorithm.Search
  , module Control.Monad.Search
  ) where

import Common
import Data.HashSet (HashSet)
import MTL
import Data.Hashable (Hashable)
import qualified Data.HashSet as HashSet
import Algorithm.Search
import Control.Monad.Search

floodFill :: (Hashable state, Witherable f)
          => (state -> f state)  -- | Generate next states
          -> state               -- | Init state
          -> HashSet state
floodFill gen = flip execAccum mempty . go gen
  where
    go :: (Hashable state, Witherable f) => (state -> f state) -> state -> Accum (HashSet state) ()
    go step start = do
      add (HashSet.singleton start)
      let thenStates = step start
      novelties <- filterA (looks . notElem) thenStates
      traverse_ (go step) novelties
