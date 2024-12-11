module Y2024.D05 () where

import Common
import MTL
import Read
import Data.List.NonEmpty (NonEmpty(..))
import GHC.IsList (fromList)
import qualified NE

type Page = Int

data Rule = Rule Page Page
  deriving (Show, Eq)

type Update = NonEmpty Page

validate :: ( MonadReader [Rule] m
            , MonadLogger m
            , MonadFail m
            )
         => Update -> m Bool
validate (p:|[]) = do
  $logInfo [i|#{p} is only page left, always correct|]
  pure True
validate (p1:|p2:ps) = do
  $logInfo [i|comparing #{p1} and #{p2}|]
  Just res <- runMaybeT $ p1 `before` p2
  if res then validate (p2:|ps) else pure False

sort :: ( MonadReader [Rule] m
        , MonadLogger m
        , MonadFail m
        )
     => Update -> m Update
sort = NE.sortByM $ \p1 p2 -> do
  Just res <- runMaybeT $ cmpM p1 p2
  pure res

cmpM :: ( MonadReader [Rule] m
        , MonadLogger m
        )
     => Page -> Page -> MaybeT m Ordering
cmpM p1 p2 = do
  mrule <- asks (find (liftA2 (||) (== Rule p1 p2) (== Rule p2 p1)))
  case mrule of
    Just (Rule x y)
      | x == p1 && y == p2 -> $logInfo "found rule" $> LT
      | otherwise -> $logInfo "found anti-rule" $> GT
    Nothing -> $logError [i|rule not found for #{p1} and #{p2}|] *> empty

-- | Synonym
before :: ( MonadReader [Rule] m
          , MonadLogger m
          )
       => Page -> Page -> MaybeT m Bool
p1 `before` p2 = cmpM p1 p2 <&> \case
  LT -> True
  _ -> False

parse :: ReadP ([Rule], [Update])
parse = do
  rules <- sepBy1 rule newline
  newline
  newline
  updates <- sepBy1 update newline
  newline
  eof
  pure (rules, updates)
  where newline = void $ char '\n'

rule :: ReadP Rule
rule = do
  p1 <- uint
  char '|'
  Rule p1 <$> uint

update :: ReadP Update
update =
  -- sepBy1 by documentation parses *one* or more occurrences
  -- safe to coerce to NonEmpty therefore
  fromList <$> sepBy1 uint (char ',')

-- part1 :: IO ()
-- part1 = do
--   contents <- input @String
--   [((rules, updates),"")] <- pure $ readP_to_S Y2024.D05.parse contents
--   validUpdates <- filterM (\up -> runNoLoggingT (runReaderT (validate up) rules)) updates
--   print @Int $ sum @[] $ mapMaybe (mid . toList) validUpdates
--
-- part2 :: IO ()
-- part2 = do
--   contents <- input @String
--   [((rules, updates),"")] <- pure $ readP_to_S Y2024.D05.parse contents
--   invalidUpdates <- filterM (\up -> not <$> runNoLoggingT (runReaderT (validate up) rules)) updates
--   sortedUpdateds <- traverse (\up -> runNoLoggingT (runReaderT (sort up) rules)) invalidUpdates
--   print @Int $ sum @[] $ mapMaybe (mid . toList) sortedUpdateds
