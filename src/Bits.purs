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

class (Eq a, BooleanAlgebra a) <= Bits a where
  xor :: a -> a -> a
  shift :: a -> Int -> a
  rotate :: a -> Int -> a
  testBit :: a -> Int -> Boolean
  popCount :: a -> Int

(.^.) :: forall a. (Bits a) => a -> a -> a
(.^.) = xor

