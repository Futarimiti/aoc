-- | Generate templates for a year
module Generate (mkYear) where

import System.Directory
import Data.Foldable
import Data.String.Interpolate (i, __i)
import Control.Monad.IO.Class
import Text.Printf
import Debug.Trace

mkYear :: MonadIO m => Int -> m ()
mkYear year = liftIO $ do
  traceM [i|Generating year #{year}...|]
  createDirectoryIfMissing True [i|src/Y#{year}|]
  for_ days $ \d -> let
    d' = printf "%02d" d :: String
    in writeFile
    [i|src/Y#{year}/D#{d'}.hs|]
    [i|module Y#{year}.D#{d'} () where

import Common

instance AOC #{year} #{d} where
  type Input #{year} #{d} = TypeError (Text "Solution to #{year}/#{d} unimplemented")
  parse :: [String] -> Input #{year} #{d}
  parse = undefined

  type Output1 #{year} #{d} = Integer
  part1 :: Input #{year} #{d} -> Output1 #{year} #{d}
  part1 = undefined

  type Output2 #{year} #{d} = Integer
  part2 :: Input #{year} #{d} -> Output2 #{year} #{d}
  part2 = undefined
      |]
  where
    days = [1..25] :: [Int]
