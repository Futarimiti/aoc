module Y2024.D03 () where

import Common
import Read
import MTL
import L (isPrefixOf)
import Prelude hiding (init)

data FSM = FSM
  { _enabled :: Bool
  , _acc     :: Int
  , _memory  :: String
  } deriving (Show, Eq)

makeLenses ''FSM

instance AOC 2024 3 where
  type Input 2024 3 = String
  parse :: [String] -> Input 2024 3
  parse = concat

  type Output1 2024 3 = Int
  part1 :: Input 2024 3 -> Output1 2024 3
  part1 mem = final ^. acc
    where init = FSM True 0 mem
          final = flip execState init $ runExceptT (execute False)

  type Output2 2024 3 = Int
  part2 :: Input 2024 3 -> Output2 2024 3
  part2 mem = final ^. acc
    where init = FSM True 0 mem
          final = flip execState init $ runExceptT (execute True)

mul :: ReadP (Int, Int)
mul = do
  _ <- string "mul("
  x <- uint
  _ <- char ','
  y <- uint
  _ <- char ')'
  pure $ (x, y)

-- | Bool to indicate wether to recognise @do@ and @don't@ instructions
--
-- Use @False@ for part 1, @True@ for part 2
execute :: (MonadState FSM m, MonadError () m) => Bool -> m ()
execute recogniseDo's = forever $ do
  myMemory <- use memory
  when (null myMemory) $ exit ()
  isEnabled <- use enabled
  if | recogniseDo's, "do()" `isPrefixOf` myMemory -> do
       enabled .= True
       memory %= drop 4
     | not isEnabled -> memory %= drop 1
     | recogniseDo's, "don't()" `isPrefixOf` myMemory -> do
       enabled .= False
       memory %= drop 7
     | (((x, y), remain):_) <- readP_to_S mul myMemory -> do
       acc += x * y
       memory .= remain
     | otherwise -> memory %= drop 1

