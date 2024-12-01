-- | Generate templates for a year
module Generate (mkYear) where

import System.Directory
import Data.Foldable
import Data.String.Interpolate (i, __i)
import Control.Monad.IO.Class
import Text.Printf

mkYear :: MonadIO m => Int -> m ()
mkYear year = liftIO $ do
  createDirectoryIfMissing True [i|src/Y#{year}|]
  for_ days $ \d -> let
    d' = printf "%02d" d :: String
    in writeFile
    [i|src/Y#{year}/D#{d'}.hs|]
    [__i|module Y#{year}.D#{d'} where

      import Common|]
  where
    days = [1..25] :: [Int]
