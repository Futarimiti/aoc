module Y2024.D10 where

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

type Topography = HashMap (V2 Int) Nat

readTopography :: String -> Topography
readTopography s = Map.fromList $ do
  let matrix = lines s
  (y, cs) <- zip [0..] matrix
  (x, c) <- zip [0..] cs
  let nat = fromIntegral $ digitToInt c
  pure (V2 x y, nat)

type Memoized k v m = MonadAccum (HashMap k v) m

evalMemoizedT :: (Monad m, Monoid w) => AccumT w m a -> m a
evalMemoizedT m = evalAccumT m mempty

evalMemoized :: (Monoid w) => Accum w a -> a
evalMemoized m = evalAccum m mempty

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

part1 :: (MonadReader Topography m) => m Nat
part1 = do
  trailheads' <- trailheads
  (Map.map (fromIntegral . length) -> scores) <- execStateT distribute mempty
  let trailheadScores = sum [scores Map.! h | h <- trailheads']
  pure trailheadScores



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

part2 :: (MonadReader Topography m, Memoized (V2 Int) Nat m) => m Nat
part2 = do
  trailheads' <- trailheads
  scores <- traverse score' trailheads'
  pure $ sum scores
