module Bits
  ( Bits
  , and
  , (.&.)
  , or
  , (.|.)
  , xor
  , (.^.)
  , complement
  , shift
  , rotate
  , testBit
  , popCount
  ) where

import Prelude

class (Eq a) <= Bits a where
  and :: a -> a -> a
  or :: a -> a -> a
  xor :: a -> a -> a
  complement :: a -> a
  shift :: a -> Int -> a
  rotate :: a -> Int -> a
  testBit :: a -> Int -> Boolean
  popCount :: a -> Int

(.&.) :: forall a. (Bits a) => a -> a -> a
(.&.) = and

(.|.) :: forall a. (Bits a) => a -> a -> a
(.|.) = or

(.^.) :: forall a. (Bits a) => a -> a -> a
(.^.) = xor
