module Y2024.D06 () where

import Linear.V2
import Common
import MTL
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Hashable
import GHC.Generics
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified NE

data Orientation = L | R | U | D
  deriving (Show, Eq, Ord, Generic)

instance Hashable Orientation

type Board a = HashMap (V2 Int) a

data Guard = Guard
  { _pos         :: V2 Int
  , _orientation :: Orientation
  } deriving (Show, Eq, Ord, Generic)

makeLenses ''Guard

instance Hashable Guard

startPos :: MonadReader (Board Char) m => m (V2 Int)
startPos = do
  board <- ask
  pure $ NE.head $ NE.fromList
    [V2 x y | (x,y) <- pascal, board ^? ix (V2 x y) == Just '^']
  where
    pascal :: [(Int, Int)]
    pascal = [(x, n - x) | n <- [1..], x <- [1 .. n - 1]]

data WalkEnd
  = GotOut
  | InLoop
  deriving (Show, Eq)

-- | Walk as a guard. Accumulate past guard clones.
-- Aborts when the guard is going to wonder outside the board,
-- or start to enter a loop in the next move.
walk :: forall m.
        ( MonadReader (Board Char) m
        , MonadState Guard m
        , MonadAccum (HashSet Guard) m
        , MonadError WalkEnd m
        )
     => m ()
walk = forever $ do
  record
  mnextChar <- runMaybeT nextChar
  case mnextChar of
    Nothing  -> exit GotOut
    Just '#' -> clockwise90
    Just _   -> moveOn
  where
    record :: m ()
    record = do
      guard' <- get
      exists <- looks (guard' `elem`)
      when exists $ exit InLoop
      add $ Set.singleton guard'
    nextChar :: MaybeT m Char
    nextChar = do
      p <- lift nextPos
      res <- preview (ix p)
      hoistMaybe res
    nextPos :: m (V2 Int)
    nextPos = do
      myPos <- use pos
      myOrientaiton <- use orientation
      let setter = case myOrientaiton of
            L -> _x -~ 1
            R -> _x +~ 1
            U -> _y -~ 1
            D -> _y +~ 1
      pure $ setter myPos
    moveOn, clockwise90 :: m ()
    moveOn = pos <~ nextPos
    clockwise90 = orientation %= \case
      L -> U
      U -> R
      R -> D
      D -> L

instance AOC 2024 6 where
  type Input 2024 6 = Board Char
  parse :: [[Char]] -> Input 2024 6
  parse inp = Map.fromList $ do
    (y, row) <- zip [0..] inp
    (x, c) <- zip [0..] row
    pure (V2 x y, c)

  type Output1 2024 6 = Int
  part1 :: Input 2024 6 -> Output1 2024 6
  part1 board = length visits
    where
      start = Guard (startPos board) U
      guards = flip execAccum Set.empty $
        flip runReaderT board $
        flip runStateT start $
        runExceptT walk
      visits = Set.map (view pos) guards

  type Output2 2024 6 = Int
  -- Took ~1hr to finish running, got correct answer anyway
  -- All's Well That Ends Well ig
  part2 :: Input 2024 6 -> Output2 2024 6
  part2 board = length loopedVariants
    where
      start = Guard (startPos board) U
      guards = flip execAccum Set.empty $ flip runReaderT board $ flip runStateT start $ runExceptT walk
      visits :: HashSet (V2 Int)
      visits = Set.map (view pos) guards
      boardVariants :: HashSet (Board Char)
      boardVariants = Set.map (\pos' -> board & ix pos' .~ '#') visits
      loopedVariants :: HashSet (Board Char)
      loopedVariants = Set.filter endsInLoop boardVariants
      endsInLoop :: Board Char -> Bool
      endsInLoop b = Left InLoop == evalAccum
        (flip runReaderT b $ flip evalStateT start $ runExceptT walk) Set.empty

