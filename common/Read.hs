{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Read
  ( module Text.ParserCombinators.ReadP
  , module Text.Read
  , module Control.Applicative
  , uint
  ) where

import Text.ParserCombinators.ReadP hiding (many)
import Text.Read (readMaybe)
import Control.Applicative hiding (optional)
import Data.Char (isDigit)

uint :: (Read a, Integral a) => ReadP a
uint = do
  digits <- some (satisfy isDigit)
  maybe pfail pure (readMaybe digits)
