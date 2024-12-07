module Y2021.D03 where

import Common

type Bin = [Bool]

readBin :: String -> Bin
readBin = map $ \case
  '0' -> False
  _ -> True

binToDec :: Bin -> Int
binToDec = foldl' (\acc b -> acc * 2 + if b then 1 else 0) 0

more1 :: [Bool] -> Bool
more1 = uncurry ((>) @Integer) . foldl' (\(t, f) b -> if b then (t + 1, f) else (t, f + 1)) (0, 0)

powerConsumption :: [Bin] -> Int
powerConsumption report = binToDec gamma * binToDec epsilon
  where (gamma, epsilon) = unzip $ map (\bs -> if more1 bs then (True, False) else (False, True)) report

lifeSupportRating :: a -> Int
lifeSupportRating _ = oxygen * co2
  where oxygen = undefined
        co2 = undefined
