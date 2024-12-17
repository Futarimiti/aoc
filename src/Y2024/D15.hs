{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Y2024.D15 () where

import Common
import Read
import Linear.V2
import Data.List.Split (splitOn)
import Data.HashMap.Lazy (HashMap, (!?))
import qualified Data.HashMap.Lazy as HashMap
import Data.Default
import MTL
import Board

type Locs = Board Integer Char
type Move = Char
type Instructions = [Move]

instance AOC 2024 15 where
  type Input 2024 15 = (Locs, Instructions)
  parse :: [String] -> Input 2024 15
  parse lns = case splitOn [""] lns of
    [mapLns, instructionsLns] -> (parseMap mapLns, parseInstructions instructionsLns)
    _                         -> error "parse fail"
    where
      parseInstructions :: [[Char]] -> Instructions
      parseInstructions = join
      parseMap :: [[Char]] -> Locs
      parseMap = fromMatrix

  type Output1 2024 15 = Integer
  part1IO :: Input 2024 15 -> IO (Output1 2024 15)
  part1IO inp = do
    let final = runBaba inp
    boxes <- HashMap.filter (== 'O') <$> final <&> HashMap.keys
    let gpsCoords = gps <$> boxes
    pure $ sum $ gpsCoords

  type Output2 2024 15 = Integer
  part2IO :: Input 2024 15 -> IO (Output2 2024 15)
  part2IO (map', ins) = do
    let final = runBaba' (fatMap, ins)
    boxes <- HashMap.filter (== '[') <$> final <&> HashMap.keys
    let gpsCoords = gps <$> boxes
    pure $ sum $ gpsCoords
    where
      fatMap = double map'

gps :: V2 Integer -> Integer
gps (V2 x y) = x + 100 * y

runBaba :: (Locs, Instructions) -> IO Locs
runBaba (locs, instructions) =
  flip execStateT locs $ runStderrLoggingT $ processAll instructions robotStartPos
  where
    robotStartPos :: V2 Integer
    robotStartPos = case HashMap.keys $ HashMap.filter (== '@') locs of
      [pos] -> pos
      _     -> error "where's bot???"

processAll :: (MonadState Locs m)
           => Instructions
           -> V2 Integer
           -> m ()
processAll [] _ = pure ()
processAll (ch:remain) robotPos = do
  successful <- push towards robotPos
  if successful then
    processAll remain (towards 1 robotPos)
  else
    processAll remain robotPos
  where towards = case ch of
          '^' -> (_y -~)
          'v' -> (_y +~)
          '>' -> (_x +~)
          _le -> (_x -~)

-- | Try to push a single-grid *thing* at a certain coord towards a direction by 1 unit.
-- Nop if the object is air or a wall, or a box stacked against a wall.
-- Returns @True@ if the move was successfully performed,
-- @False@ if bumped into a wall.
--
-- Example - push upwards: @push (_y -~) (V2 3 4)@
push :: forall m n.
        ( MonadState Locs m
        , n ~ Integer
        )
     => (n -> V2 n -> V2 n)
     -> V2 n
     -> m Bool
push towards coord = do
  mobject <- gets (!? coord)
  case mobject of
    Nothing     -> pure True
    Just '#'    -> pure False
    Just object -> do
      let pusheeCoord = 1 `towards` coord
      successful <- push towards pusheeCoord
      when successful $ do
        let newCoord = pusheeCoord
        modify $ HashMap.delete coord
        modify $ HashMap.insert newCoord object
      pure successful

-- part 2

double :: Locs -> Locs
double = HashMap.foldrWithKey'
  (\(V2 x y) -> \case
    '@' -> HashMap.insert (V2 (2 * x) y) '@'
    'O' -> HashMap.insert (V2 (2 * x) y) '[' . HashMap.insert (V2 (2 * x + 1) y) ']'
    '#' -> HashMap.insert (V2 (2 * x) y) '#' . HashMap.insert (V2 (2 * x + 1) y) '#')
  HashMap.empty

data Dir = U | D | L | R
  deriving Show

push' :: forall m. (MonadState Locs m)
      => Dir
      -> V2 Integer
      -> m Bool
push' dir coord = do
  let move = case dir of
        U -> _y -~ 1
        D -> _y +~ 1
        L -> _x -~ 1
        R -> _x +~ 1
  mobject <- gets (!? coord)
  case mobject of
    Nothing  -> pure True
    Just '#' -> pure False
    Just '@' -> do
      let pusheeCoord = move coord
      successful <- push' dir pusheeCoord
      when successful $ do
        let newCoord = pusheeCoord
        modify $ HashMap.delete coord
        modify $ HashMap.insert newCoord '@'
      pure successful
    Just '[' -> do
      let
        pusheeCoords :: [V2 Integer]
        pusheeCoords = case dir of
            U -> [coord & _y -~ 1, coord & _y -~ 1 & _x +~ 1]
            D -> [coord & _y +~ 1, coord & _y +~ 1 & _x +~ 1]
            R -> [coord & _x +~ 1]
            L -> [coord & _x -~ 1]
      (and -> allSuccessful) <- traverse (canPush' dir) pusheeCoords
      when allSuccessful $ do
        traverse (push' dir) pusheeCoords
        modify $ HashMap.delete coord
               . HashMap.delete (coord & _x +~ 1)
        modify $ case pusheeCoords of
          [l, r] -> HashMap.insert l '[' . HashMap.insert r ']'
          [l]    -> HashMap.insert l '[' . HashMap.insert (l & _x +~ 1) ']'
      pure allSuccessful
    Just ']' -> do
      let
        pusheeCoords :: [V2 Integer]
        pusheeCoords = case dir of
            U -> [coord & _y -~ 1 & _x -~ 1, coord & _y -~ 1]
            D -> [coord & _y +~ 1 & _x -~ 1, coord & _y +~ 1]
            L -> [coord & _x -~ 1]
            R -> [coord & _x +~ 1]
      (and -> allSuccessful) <- traverse (canPush' dir) pusheeCoords
      when allSuccessful $ do
        traverse (push' dir) pusheeCoords
        modify $ HashMap.delete coord
               . HashMap.delete (coord & _x -~ 1)
        modify $ case pusheeCoords of
          [l, r] -> HashMap.insert l '[' . HashMap.insert r ']'
          [r]    -> HashMap.insert (r & _x -~ 1) '[' . HashMap.insert r ']'
      pure allSuccessful

-- | Dry-run variant of @push'@
canPush' :: forall m. (MonadState Locs m)
         => Dir
         -> V2 Integer
         -> m Bool
canPush' dir coord = do
  let move = case dir of
        U -> _y -~ 1
        D -> _y +~ 1
        L -> _x -~ 1
        R -> _x +~ 1
  mobject <- gets (!? coord)
  case mobject of
    Nothing  -> pure True
    Just '#' -> pure False
    Just '@' -> do
      let pusheeCoord = move coord
      successful <- canPush' dir pusheeCoord
      when successful $ do
        let newCoord = pusheeCoord
        modify $ HashMap.delete coord
        modify $ HashMap.insert newCoord '@'
      pure successful
    Just '[' -> do
      let
        pusheeCoords :: [V2 Integer]
        pusheeCoords = case dir of
            U -> [coord & _y -~ 1, coord & _y -~ 1 & _x +~ 1]
            D -> [coord & _y +~ 1, coord & _y +~ 1 & _x +~ 1]
            R -> [coord & _x +~ 1]
            L -> [coord & _x -~ 1]
      (and -> allSuccessful) <- traverse (canPush' dir) pusheeCoords
      pure allSuccessful
    Just ']' -> do
      let
        pusheeCoords :: [V2 Integer]
        pusheeCoords = case dir of
            U -> [coord & _y -~ 1 & _x -~ 1, coord & _y -~ 1]
            D -> [coord & _y +~ 1 & _x -~ 1, coord & _y +~ 1]
            L -> [coord & _x -~ 1]
            R -> [coord & _x +~ 1]
      (and -> allSuccessful) <- traverse (canPush' dir) pusheeCoords
      pure allSuccessful

runBaba' :: (Locs, Instructions) -> IO Locs
runBaba' (locs, instructions) =
  flip execStateT locs $ runStderrLoggingT $ processAll' instructions robotStartPos
  where
    robotStartPos :: V2 Integer
    robotStartPos = case HashMap.keys $ HashMap.filter (== '@') locs of
      [pos] -> pos
      _     -> error "where's bot???"

processAll' :: ( MonadState Locs m
               )
            => Instructions
            -> V2 Integer
            -> m ()
processAll' [] _ = pure ()
processAll' (ch:remain) robotPos = do
  successful <- push' dir robotPos
  if successful then
    processAll' remain (towards 1 robotPos)
  else
    processAll' remain robotPos
  where towards = case ch of
          '^' -> (_y -~)
          'v' -> (_y +~)
          '>' -> (_x +~)
          _le -> (_x -~)
        dir = case ch of
          '^' -> U
          'v' -> D
          '>' -> R
          _le -> L

