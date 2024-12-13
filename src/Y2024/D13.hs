module Y2024.D13 () where

import Common
import Linear.V2
import Read

data ClawMachine = ClawMachine
  { _buttonA :: V2 Integer
  , _buttonB :: V2 Integer
  , _prize   :: V2 Integer
  } deriving Show

makeLenses ''ClawMachine

instance AOC 2024 13 where
  type Input 2024 13 = [ClawMachine]
  readp :: ReadP (Input 2024 13)
  readp = sepBy1 clawMachine (string "\n\n")
    where
      clawMachine :: ReadP ClawMachine
      clawMachine = do
        a <- button 'A'
        char '\n'
        b <- button 'B'
        char '\n'
        p <- pprize
        pure $ ClawMachine a b p
      button :: Char -> ReadP (V2 Integer)
      button name = do
        string "Button "
        char name
        string ": X+"
        x <- uint
        string ", Y+"
        y <- uint
        pure $ V2 x y
      pprize :: ReadP (V2 Integer)
      pprize = do
        string "Prize: X="
        x <- uint
        string ", Y="
        y <- uint
        pure $ V2 x y

  type Output1 2024 13 = Integer
  part1 :: Input 2024 13 -> Output1 2024 13
  part1 = sum . fmap cost . mapMaybe hack

  type Output2 2024 13 = Integer
  part2 :: Input 2024 13 -> Output2 2024 13
  part2 = sum . fmap cost . mapMaybe (hack . (prize +~ 10000000000000))

cost :: Num a => (a, a) -> a
cost (a, b) = 3 * a + b

hack :: ClawMachine -> Maybe (Integer, Integer)
hack (ClawMachine (V2 xa ya) (V2 xb yb) (V2 xp yp))
  | r1 == 0 && r2 == 0 = Just (x, y)
  | otherwise = Nothing
  where
    (y, r1) = (ya * xp - xa * yp) `divMod` (ya * xb - xa * yb)
    (x, r2) = (xp - xb * y) `divMod` xa
