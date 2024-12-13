{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RequiredTypeArguments #-}

-- | My custom prelude
module Common
  (
  -- * Running AOC
    AOC (..)
  , run
  -- * Extra utilities
  , counts
  , count
  , countA
  , andM
  , orM
  , allM
  , anyM
  -- * Re-exports
  , module Data.String.Interpolate
  , module Data.Function
  , module Control.Monad
  , module Control.Applicative
  , module Data.Functor
  , module Data.Functor.Compose
  , module Control.Lens
  , module Control.Monad.IO.Class
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Traversable
  , module Witherable
  , module GHC.TypeLits
  , module Numeric.Natural
  , module Data.Kind
  , module Data.Proxy
  , module Text.Printf
  , module Debug.Trace
  , module Debug.Trace.Dbg
  , module Data.Ord
  ) where
import Data.Functor.Compose
import Debug.Trace.Dbg
import Data.Ord
import Debug.Trace
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
import Numeric.Natural
import GHC.TypeLits
import Data.Kind
import Data.Proxy
import Text.Printf
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as P

type AOC :: Nat -> Nat -> Constraint
class AOC year day where
  type Input year day
  parse :: [String] -> Input year day
  default parse :: Show (Input year day) => [String] -> Input year day
  parse lns = case find good results of
    Just (r, "") -> r
    _ -> error [i|no parse, here's parse result: #{results}|]
    where
      p = do
        ret <- readp @year @day
        P.optional newline
        P.eof
        pure ret
      results = P.readP_to_S p (unlines lns)
      good (_, "") = True
      good _ = False
      newline = P.char '\n'

  readp :: ReadP (Input year day)
  readp = parse @year @day . lines <$> P.look

  type Output1 year day
  type instance Output1 _ _ = Integer
  part1 :: Input year day -> Output1 year day
  part1 = undefined
  part1IO :: Input year day -> IO (Output1 year day)
  part1IO = pure . part1 @year @day

  type Output2 year day
  type instance Output2 _ _ = Integer
  part2 :: Input year day -> Output2 year day
  part2 = undefined
  part2IO :: Input year day -> IO (Output2 year day)
  part2IO = pure . part2 @year @day

  {-# MINIMAL (parse | readp), (part1 | part1IO), (part2 | part2IO) #-}

type OutputOn :: Nat -> Nat -> Nat -> Type
type family OutputOn year day part = output where
  OutputOn    year day 1 = Output1 year day
  OutputOn    year day 2 = Output2 year day

class AOC year day => RunAOC year day part where
  run' :: IO (OutputOn year day part)

instance (AOC year day, KnownNat year, KnownNat day) => RunAOC year day 1 where
  run' = part1IO @year @day =<< getInput @year @day

instance (AOC year day, KnownNat year, KnownNat day) => RunAOC year day 2 where
  run' = part2IO @year @day =<< getInput @year @day

-- |
-- >>> run 2021  1    2
--         ^year ^day ^part
run :: forall year day part
    -> (RunAOC year day part)
    => IO (OutputOn year day part)
run y d p = run' @y @d @p

getInput :: forall year day. (AOC year day, KnownNat year, KnownNat day)
         => IO (Input year day)
getInput = do
  path <- getDataFileName [i|#{year}/#{printf "%02d" day :: String}.txt|]
  inputStr <- readFile path
  pure $ parse @year @day (lines inputStr)
  where
    year = natVal $ Proxy @year
    day = natVal $ Proxy @day

-- -- | Mode value, in the statistical sense
-- mode :: Ord a => [a] -> a
-- mode = head . maximumBy (comparing length) . group . sort

-- | Count occurrences of elements within a @'Foldable'@,
-- summarising in a @Map@.
counts :: (Num n, Ord a, Foldable t) => t a -> Map a n
counts = foldr (\x -> Map.insertWith (+) x 1) mempty

-- | Count occurrences of elements within a @'Foldable'@ and @'MonadPlus'@.
count :: (Num n, MonadPlus t, Foldable t) => (a -> Bool) -> t a -> n
count p = fromIntegral . length . mfilter p

-- | Monadic generalisation of @count@
countA :: (Num n, Witherable t, Applicative m) => (a -> m Bool) -> t a -> m n
countA p = fmap (fromIntegral . length) . filterA p

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
