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
  , popCountLookupTable
  ) where

import Prelude
import Data.Array ((..))
import Data.Array.Unsafe (unsafeIndex)
import qualified Data.Int.Bits as IntBits
import UnsignedInts (UInt8(), intToByte, byteToInt)

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

popCountLookupTable :: Array Int
popCountLookupTable = map countBits (0 .. 255) where
  countNibble = [0, 1, 1, 2, 1, 2, 2, 3,
                 1, 2, 2, 3, 2, 3, 3, 4]
  countBits x = countNibble `unsafeIndex` (IntBits.zshr x 4) + countNibble `unsafeIndex` (IntBits.(.&.) x 0x0F)

instance bitsUInt8 :: Bits UInt8 where
  and a b = intToByte $ IntBits.(.&.) (byteToInt a) (byteToInt b)
  or a b = intToByte $ IntBits.(.|.) (byteToInt a) (byteToInt b)
  xor a b = intToByte $ IntBits.(.^.) (byteToInt a) (byteToInt b)
  complement = intToByte <<< IntBits.complement <<< byteToInt
  shift x n | n > 0 = intToByte $ IntBits.zshr (byteToInt x) n
            | n < 0 = intToByte $ IntBits.shl (byteToInt x) (- n)
            | otherwise = x
  rotate x n = intToByte masked where
    ix = (byteToInt x)
    repeated = IntBits.(.|.) (IntBits.shl ix 8) ix
    shifted = IntBits.zshr repeated (n `mod` 8)
    masked = IntBits.(.&.) shifted 0xFF
  testBit x n = IntBits.(.&.) shifted 1 == 1 where
    shifted = IntBits.zshr (byteToInt x) n
  popCount = unsafeIndex popCountLookupTable <<< byteToInt
