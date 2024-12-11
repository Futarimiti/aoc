{-# LANGUAGE NoOverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Y2024.D07 () where

import Common
import NE
import L
import Read

-- I greatly suspect part 2 expands on this
-- operations = [(+), (*)]

feasible :: (Num a, Eq a)
         => [a -> a -> a]  -- | possible operations
         -> NonEmpty a     -- | operands, with fixed order
         -> a              -- | expected result
         -> Bool           -- | feasible?
feasible _ (x:|[]) res = x == res
feasible ops (x:|y:xs) res = not $ null [() | op <- ops, feasible ops (op x y:|xs) res]

-- | One line
parse :: forall a. (Read a, Integral a) => ReadP (a, NonEmpty a)
parse = do
  res <- uint
  _ <- string ": "
  xs <- sepBy1 uint (char ' ')
  pure (res, fromList xs)  -- safe

