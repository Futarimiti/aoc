module Y2024.D14 () where

import Common
import Linear.V2
import Read
import MTL
import qualified L
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Control.Monad.Zip
import GHC.IsList (IsList)
import qualified Data.HashSet as HashSet
import Data.List (isInfixOf)

data Robot = Robot
  { _pos      :: V2 Integer  -- y: from top
  , _velocity :: V2 Integer  -- positive y: moving down
  } deriving Show

makeLenses ''Robot

instance AOC 2024 14 where
  type Input 2024 14 = (Integer, Integer, [Robot])
  readp :: ReadP (Integer, Integer, [Robot])
  readp = do
    robots <- sepBy1 robot (char '\n')
    pure (101, 103, robots)
    where
      robot :: ReadP Robot
      robot = do
        myPos <- ppos
        char ' '
        myVel <- pvelocity
        pure $ Robot myPos myVel
      ppos, pvelocity :: (Read i, Integral i) => ReadP (V2 i)
      ppos = do
        string "p="
        x <- uint
        char ','
        y <- uint
        pure (V2 x y)
      pvelocity = do
        string "v="
        x <- uint <|> nint
        char ','
        y <- uint <|> nint
        pure (V2 x y)

  type Output1 2024 14 = Integer
  part1 :: Input 2024 14 -> Output1 2024 14
  part1 (width, height, inp) = product @[] [q1, q2, q3, q4]
    where
      final :: [Robot]
      final = moves 100 inp (width, height)
      q1, q2, q3, q4 :: Integer
      (q1, q2, q3, q4) = foldr (\(view pos -> V2 x y) -> if
        | x > width `div` 2 && y < height `div` 2 -> _1 +~ 1
        | x < width `div` 2 && y < height `div` 2 -> _2 +~ 1
        | x < width `div` 2 && y > height `div` 2 -> _3 +~ 1
        | x > width `div` 2 && y > height `div` 2 -> _4 +~ 1
        | otherwise                               -> id) (0,0,0,0) final

  -- | Alright, alright, alright...
  -- part 2 today is not designed to be solved in code, you need to FOCUS
  type Output2 2024 14 = ()
  part2IO :: Input 2024 14 -> IO (Output2 2024 14)
  part2IO (w, h, r) = do
    let transformations = drop 7000 $ iterate (moves' 1) r
        coords :: [[V2 Integer]] = getCompose . fmap (view pos) . Compose $ transformations
        displays :: [[String]] = fmap (showBoard (w, h)) coords
    putStrLn "start with 7000"  -- CHEAT!
    keepDisplay displays
    where moves' n robots = moves n robots (w, h)

move :: forall m.
        ( MonadReader (Integer, Integer) m  -- | max width & height
        )
     => Robot
     -> m (V2 Integer)
move robot = do
  dest <- maybeTeleport (robot^.pos + robot^.velocity)
  pure dest
  where
    maybeTeleport :: V2 Integer -> m (V2 Integer)
    maybeTeleport (V2 x y) = do
      (maxX, maxY) <- ask
      let
        x' = if
          | x < 0     -> maxX + x
          | x >= maxX -> x - maxX
          | otherwise -> x
        y' = if
          | y < 0     -> maxY + y
          | y >= maxY -> y - maxY
          | otherwise -> y
      pure $ V2 x' y'

moves :: forall m t. (MonadReader (Integer, Integer) m, Traversable t)
      => Integer -> t Robot -> m (t Robot)
moves n = traverse (move' n)
  where
    move' :: Integer -> Robot -> m Robot
    move' 0 r  = pure r
    move' n' r = do
      wh <- ask
      move' (n' - 1) (r & pos .~ (move r wh))

showBoard :: (Integer, Integer) -> [V2 Integer] -> [String]
showBoard (width, height) coords = do
  y <- [0 .. height - 1]
  pure $ do
    x <- [0 .. width - 1]
    pure $ cell x y
  where
    s = HashSet.fromList coords
    cell x y | (V2 x y) `elem` s = '*'
             | otherwise = ' '

-- | Display strings, wait for user *enter* then next
keepDisplay :: [[String]] -> IO ()
keepDisplay = go 1
  where
    go :: Integer -> [[String]] -> IO ()
    go curr [] = putStrLn [i|done, finish at #{curr}|]
    go curr (lns:rest) = do
      when (curr `mod` 100 == 0) $ do
        print curr
      when (has10consecutive lns) $ do
        putStr "\ESC[2J"
        traverse_ putStrLn lns
        putStrLn [i|Move #{curr}, hit enter to continue...|]
        void getLine
      go (curr + 1) rest

has10consecutive :: [String] -> Bool
has10consecutive = any ((replicate 10 '*') `isInfixOf`)

