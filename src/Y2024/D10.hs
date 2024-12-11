module Y2024.D10 () where

import Common
import MTL
import Board
import GHC.TypeNats
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Data.Char (digitToInt)
import Debug.Trace
import Debug.Trace.Dbg
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import Memoize

type Topography = HashMap (V2 Int) Nat

trailheads :: (MonadReader Topography m) => m [V2 Int]
trailheads = Map.keys . Map.filter (== 0) <$> ask

-- current point & set of 9's this point leads to
type Scores = HashMap (V2 Int) (HashSet (V2 Int))

-- | "Landslide" from all n's (start from 9)
distribute :: forall m. (MonadState Scores m, MonadReader Topography m)
           => m ()
distribute = for_ ([9,8..1] :: [Nat]) $ \n -> do
  coords <- asks $ Map.keys . Map.filter (== n)
  coords & traverse_ \coord -> do
    my9's <- if n == 9
      then pure $ Set.singleton coord
      else gets $ Map.findWithDefault mempty coord
    myNeighbours <- lowerNeighbours coord
    for_ myNeighbours \neighbour -> do
      at neighbour %= \case
        Nothing -> Just my9's
        Just s -> Just $ s <> my9's
  where
    lowerNeighbours :: V2 Int -> m [V2 Int]
    lowerNeighbours coords = do
      myHeight <- asks (Map.! coords)
      let neighbouringCoords = ($ coords) <$>
            [_x %~ (+1), _y %~ (+1), _x %~ subtract 1, _y %~ subtract 1]
      neighbouringCoords & filterA \c -> asks (Map.lookup c) <&> \case
        Just h | succ h == myHeight -> True
        _                           -> False



-- | score = # of 9's this pos leads to
score' :: forall m. (MonadReader Topography m, Memoized (V2 Int) Nat m)
      => V2 Int  -- | point standing on
      -> m Nat   -- | score
score' coords = coords & retrieveOrM do
  myHeight <- asks (Map.! coords)
  if myHeight == 9 then pure 1 else do
    higherNeighbours <- lookaround
    theirScores <- traverse score' higherNeighbours
    pure $ sum theirScores
  where
    -- look for next steps to land on
    lookaround :: m [V2 Int]
    lookaround = do
      myHeight <- asks (Map.! coords)
      let neighbouringCoords = ($ coords) <$>
            [_x %~ (+1), _y %~ (+1), _x %~ subtract 1, _y %~ subtract 1]
      neighbouringCoords & filterA \c -> asks (Map.lookup c) <&> \case
        Just h | h == succ myHeight -> True
        _                           -> False

instance AOC 2024 10 where
  type Input 2024 10 = Topography
  parse :: [[Char]] -> Input 2024 10
  parse matrix = Map.fromList $ do
    (y, cs) <- zip [0..] matrix
    (x, c) <- zip [0..] cs
    let nat = fromIntegral $ digitToInt c
    pure (V2 x y, nat)

  type Output1 2024 10 = Nat
  part1 :: Input 2024 10 -> Output1 2024 10
  part1 = runReader do
    trailheads' <- trailheads
    (Map.map (fromIntegral . length) -> scores) <- execStateT distribute mempty
    let trailheadScores = sum [scores Map.! h | h <- trailheads']
    pure trailheadScores

  type Output2 2024 10 = Nat
  part2 :: Input 2024 10 -> Output2 2024 10
  part2 = evalMemoized . runReaderT do
    trailheads' <- trailheads
    scores <- traverse score' trailheads'
    pure $ sum scores
