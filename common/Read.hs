{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Read
  ( module Text.ParserCombinators.ReadP
  , module Text.Read
  , module Control.Applicative
  -- | Common combinators
  , uint
  , nint
  ) where

import Text.ParserCombinators.ReadP hiding (many, count)
import Text.Read (readMaybe)
import Control.Applicative hiding (optional)
import Data.Char (isDigit)

-- | Unsigned integer
uint :: (Read a, Integral a) => ReadP a
uint = do
  digits <- some (satisfy isDigit)
  maybe pfail pure (readMaybe digits)

-- | Negative signed integer
nint :: (Read a, Integral a) => ReadP a
nint = do
  char '-'
  n <- uint
  pure (negate n)
