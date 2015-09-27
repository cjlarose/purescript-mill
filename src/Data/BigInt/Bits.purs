module Data.BigInt.Bits
  ( (.&.)
  , (.|.)
  , (.^.)
  , complement
  , shiftLeft
  , shiftRight
  ) where

import Data.BigInt (BigInt())

foreign import and :: BigInt -> BigInt -> BigInt

(.&.) = and

foreign import or :: BigInt -> BigInt -> BigInt

(.|.) = or

foreign import xor :: BigInt -> BigInt -> BigInt

(.^.) = xor

foreign import not :: BigInt -> BigInt

complement = not

foreign import shiftLeft :: BigInt -> Int -> BigInt
foreign import shiftRight :: BigInt -> Int -> BigInt
