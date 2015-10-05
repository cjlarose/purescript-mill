module Bits
  ( Bits
  , xor
  , (.^.)
  , shift
  , rotate
  , testBit
  , popCount
  ) where

import Prelude

-- | `Bits` represents a finite sequence of 0s and 1s. In addition to the laws
-- | of BooleanAlgebra, instances of `Bits` must satify the following laws:
-- |
-- | * `x .^. y == (x || y) && (not (x && y))`
class (Eq a, BooleanAlgebra a) <= Bits a where
  xor :: a -> a -> a
  shift :: a -> Int -> a
  rotate :: a -> Int -> a
  testBit :: a -> Int -> Boolean
  popCount :: a -> Int

infixl 10 .^.

-- | `(.^.)` is an alias for `xor`
(.^.) :: forall a. (Bits a) => a -> a -> a
(.^.) = xor
