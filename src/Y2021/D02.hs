module Y2021.D02 () where

import Common
import MTL
import Linear.V2

forward, upward, downward :: (R2 v2, MonadState (v2 n) m, Num n) => n -> m ()
forward = (_x +=)
upward = (_y +=)
downward = (_y -=)

-- part1
exec :: (Num n, MonadState (V2 n) m)
     => String -- direction
     -> n      -- steps
     -> m ()
exec = \case
  "forward" -> forward
  "up"      -> upward
  _down     -> downward

-- part2

data Submarine a = Submarine
  { _pos :: V2 a
  , _aim :: a
  } deriving (Show)

makeLenses ''Submarine

instance R1 Submarine where
  _x = pos . _x

instance R2 Submarine where
  _xy = pos . _xy

exec' :: (MonadState (Submarine n) m, Num n)
      => String -- direction
      -> n      -- steps
      -> m ()
exec' direction steps = case direction of
  "forward" -> do
    currAim <- use aim
    forward steps
    downward $ steps * currAim
  "up"      -> aim -= steps
  _down     -> aim += steps

instance AOC 2021 2 where
  type Input 2021 2 = [(String, Integer)]
  parse :: [String] -> Input 2021 2
  parse = fmap $ \case
    (words -> [direction, read -> steps]) -> (direction, steps)
    _ -> error "no parse"

  type Output1 2021 2 = Integer
  part1 :: Input 2021 2 -> Output1 2021 2
  part1 instructions = abs $ x * y
    where
      final :: V2 Integer
      final = flip execState 0 $ traverse (uncurry exec) instructions
      x = final ^. _x
      y = final ^. _y

  type Output2 2021 2 = Integer
  part2 :: Input 2021 2 -> Output2 2021 2
  part2 instructions = abs $ x * y
    where
      final :: Submarine Integer
      final = flip execState (Submarine 0 0) $ traverse (uncurry exec') instructions
      x = final ^. _x
      y = final ^. _y
