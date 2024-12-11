{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2024.D05 () where

import Common
import MTL
import Read
import Data.List.NonEmpty (NonEmpty(..))
import GHC.IsList (fromList)
import qualified NE
import Data.List.Split (splitOn)

type Page = Int

data Rule = Rule Page Page
  deriving (Show, Eq)

type Update = NonEmpty Page

validate :: ( MonadReader [Rule] m
            , MonadFail m
            )
         => Update -> m Bool
validate (p:|[]) = do
  traceM [i|#{p} is only page left, always correct|]
  pure True
validate (p1:|p2:ps) = do
  traceM [i|comparing #{p1} and #{p2}|]
  Just res <- runMaybeT $ p1 `before` p2
  if res then validate (p2:|ps) else pure False

sort :: ( MonadReader [Rule] m
        , MonadFail m
        )
     => Update -> m Update
sort = NE.sortByM $ \p1 p2 -> do
  Just res <- runMaybeT $ cmpM p1 p2
  pure res

cmpM :: (MonadReader [Rule] m) => Page -> Page -> MaybeT m Ordering
cmpM p1 p2 = do
  mrule <- asks (find (liftA2 (||) (== Rule p1 p2) (== Rule p2 p1)))
  case mrule of
    Just (Rule x y)
      | x == p1 && y == p2 -> trace "found rule" $ pure LT
      | otherwise -> trace "found anti-rule" $ pure GT
    Nothing -> trace [i|rule not found for #{p1} and #{p2}|] empty

-- | Synonym
before :: (MonadReader [Rule] m) => Page -> Page -> MaybeT m Bool
p1 `before` p2 = cmpM p1 p2 <&> \case
  LT -> True
  _ -> False

instance AOC 2024 5 where
  type Input 2024 5 = ([Rule], [Update])
  readp = do
    rules <- sepBy1 rule newline
    newline
    newline
    updates <- sepBy1 update newline
    newline
    eof
    pure (rules, updates)
    where
      newline = void $ char '\n'
      rule = do
        p1 <- uint
        char '|'
        Rule p1 <$> uint
      update :: ReadP Update
      update =
        -- sepBy1 by documentation parses *one* or more occurrences
        -- safe to coerce to NonEmpty therefore
        fromList <$> sepBy1 uint (char ',')

  type Output1 2024 5 = Int
  part1 :: Input 2024 5 -> Output1 2024 5
  part1 (rules, updates) = let
   Just validUpdates = filterM (\up -> runReaderT (validate up) rules) updates
   in sum $ mapMaybe (mid . toList) validUpdates

  type Output2 2024 5 = Int
  part2 :: Input 2024 5 -> Output2 2024 5
  part2 (rules, updates) = let
    Just invalidUpdates = filterM (\up -> not <$> runReaderT (validate up) rules) updates
    Just sortedUpdateds = traverse (\up -> runReaderT (sort up) rules) invalidUpdates
    in sum @[] $ mapMaybe (mid . toList) sortedUpdateds

