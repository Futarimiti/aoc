{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Y2024.D03 where

import Common
import MTL
import Read
import Data.List (isPrefixOf)
import Data.Char

data FSM = FSM
  { _enabled :: Bool
  , _acc     :: Int
  , _memory  :: String
  } deriving (Show, Eq)

makeLenses ''FSM

-- | mul instruction
mul :: ReadP (Int, Int)
mul = do
  string "mul"
  between (char '(') (char ')') $ do
    x <- int
    char ','
    y <- int
    pure (x, y)
  where
    int :: ReadP Int
    int = read <$> some (satisfy isDigit)

-- | do, don't instructions
do', don't :: ReadP ()
do' = void $ string "do()"
don't = void $ string "don't()"

-- | Bool indicates wether to recognise @do@ and @don't@ instructions
--
-- Use @False@ for part 1, @True@ for part 2
execute :: (MonadState FSM m, MonadError () m) => Bool -> m ()
execute recogniseDo = forever $ do
  myMemory <- gets (view memory)
  case myMemory of
    [] -> exit ()
    'd':_ | recogniseDo, (((), remain):_) <- readP_to_S don't myMemory -> do
      enabled .= False
      memory .= remain
          | recogniseDo, (((), remain):_) <- readP_to_S do' myMemory -> do
      enabled .= True
      memory .= remain
    'm':_ | (((x, y), remain):_) <- readP_to_S mul myMemory -> do
      isEnabled <- gets (view enabled)
      when isEnabled $ acc += x * y
      memory .= remain
    _ -> memory %= drop 1
