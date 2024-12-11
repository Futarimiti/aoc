module Memoize where

import MTL
import Data.HashMap.Strict
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map

type Memoized k v = MonadAccum (HashMap k v)

evalMemoized :: (Monoid w) => Accum w a -> a
evalMemoized m = evalAccum m mempty

evalMemoizedT :: (Monad m, Monoid w) => AccumT w m a -> m a
evalMemoizedT m = evalAccumT m mempty

-- | Recommend usage (need BlockArguments): @v <- k & retrieveOrM do ...@
retrieveOrM :: (Memoized k v m, Hashable k) => m v -> k -> m v
retrieveOrM def k = do
  mv <- looks (Map.lookup k)
  case mv of
    Just v  -> pure v
    Nothing -> do
      v <- def
      add (Map.singleton k v)
      def

