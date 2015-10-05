module Bits
  ( Bits
  , xor
  , (.^.)
  , shift
  , rotate
  , testBit
  , popCount
  , popCountLookupTable
  ) where

import Prelude
import Data.Array ((..))
import Data.Array.Unsafe (unsafeIndex)
import qualified Data.BigInt as BigInt
import qualified Data.Int.Bits as IntBits

import UnsignedInts (UInt8(), intToByte, byteToInt)
import Bytes (Bytes, toBigInt, fromBigInt)
import LargeKey (LargeKey(..))
import qualified Data.BigInt.Bits as BigIntBits

class (Eq a, BooleanAlgebra a) <= Bits a where
  xor :: a -> a -> a
  shift :: a -> Int -> a
  rotate :: a -> Int -> a
  testBit :: a -> Int -> Boolean
  popCount :: a -> Int

(.^.) :: forall a. (Bits a) => a -> a -> a
(.^.) = xor

popCountLookupTable :: Array Int
popCountLookupTable = map countBits (0 .. 255) where
  countNibble = [0, 1, 1, 2, 1, 2, 2, 3,
                 1, 2, 2, 3, 2, 3, 3, 4]
  countBits x = countNibble `unsafeIndex` (IntBits.zshr x 4) + countNibble `unsafeIndex` (IntBits.(.&.) x 0x0F)

instance bitsUInt8 :: Bits UInt8 where
  xor a b = intToByte $ IntBits.(.^.) (byteToInt a) (byteToInt b)
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

instance bitsLargeKey :: (Bits a, Bits b, Bytes a, Bytes b, Bytes (LargeKey a b)) => Bits (LargeKey a b) where
  xor x y = fromBigInt $ BigIntBits.(.^.) (toBigInt x) (toBigInt y)
  shift x n = fromBigInt $ BigIntBits.shiftRight (toBigInt x) n
  rotate x n = fromBigInt masked where
    bigx = (toBigInt x)
    mask = toBigInt $ top `asTypeOf` x
    shiftAmount = mask + (one :: BigInt.BigInt)
    repeated = BigIntBits.(.|.) (shiftAmount * bigx) bigx
    shifted = BigIntBits.shiftRight repeated n
    masked = BigIntBits.(.&.) shifted mask
  testBit x n = (shifted BigIntBits..&. one) == one where
    one = BigInt.fromInt 1
    shifted = BigIntBits.shiftRight (toBigInt x) n
  popCount (LargeKey a b) = popCount a + popCount b
