{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Y2024.D03 (execute, FSM (..)) where

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
