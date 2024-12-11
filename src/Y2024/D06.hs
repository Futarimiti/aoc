module Y2024.D06 () where

import Linear.V2
import Common
import MTL
import Board
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Hashable
import GHC.Generics
import Data.HashSet as Set
import Debug.Trace

data Orientation = L | R | U | D
  deriving (Show, Eq, Ord, Generic)

instance Hashable Orientation

data Guard = Guard
  { _pos         :: V2 Int
  , _orientation :: Orientation
  } deriving (Show, Eq, Ord, Generic)

instance Hashable Guard

startPos :: MonadReader (Board Char) m => m (V2 Int)
startPos = do
  board <- ask
  pure $ head [V2 x y | (x,y) <- pascal, board ^? ix (V2 x y) == Just '^']
  where
    pascal :: [(Int, Int)]
    pascal = [(x, n - x) | n <- [1..], x <- [1 .. n - 1]]

makeLenses ''Guard

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

part1 :: IO ()
part1 = do
  contents <- input
  let board = Board.fromList $ lines contents
      start = Guard (startPos board) U
      guards = flip execAccum Set.empty $ flip runReaderT board $ flip runStateT start $ runExceptT walk
  let visits = Set.map (view pos) guards
  print (length visits)

type Set = Set.HashSet

-- Took ~1hr to finish running, got correct answer anyway
-- All's Well That Ends Well ig
part2 :: IO ()
part2 = do
  contents <- input
  let board = Board.fromList $ lines contents
      start = Guard (startPos board) U
      guards = flip execAccum Set.empty $ flip runReaderT board $ flip runStateT start $ runExceptT walk
  traceM "got guards"
  let visits :: Set (V2 Int)
      visits = Set.map (view pos) guards
      boardVariants :: Set (Board Char)
      boardVariants = Set.map (\pos' -> board & ix pos' .~ '#') visits
  traceM "got variants"
  let loopedVariants :: Set (Board Char)
      loopedVariants = Set.filter endsInLoop boardVariants
      endsInLoop :: Board Char -> Bool
      endsInLoop b = trace "checking a board" $
        Left InLoop == evalAccum (flip runReaderT b $ flip evalStateT start $ runExceptT walk) Set.empty
  print (length loopedVariants)
