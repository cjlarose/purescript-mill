module UnsignedInts
  ( UInt8()
  , UInt16()
  , UInt32()
  , UInt64()
  , UInt128()
  , intToByte
  , byteToInt
  ) where

import Prelude
import Data.Array ((..))
import Data.Array.Unsafe (unsafeIndex)
import Data.Int (floor)
import Data.Int.Bits ((.&.), (.|.), complement)
import qualified Data.BigInt as BigInt
import Test.QuickCheck.Arbitrary (arbitrary, Arbitrary)
import qualified Data.Int.Bits as IntBits

import Integral (Integral, toBigInt, fromBigInt)
import Bits (Bits)
import LargeKey (LargeKey(..))

data UInt8 = UInt8 Int

byteToInt :: UInt8 -> Int
byteToInt (UInt8 x) = x

intToByte :: Int -> UInt8
intToByte x = UInt8 (x .&. 0xFF)

instance showUInt8 :: Show UInt8 where
  show (UInt8 x) = "UInt8 " ++ show x

instance boundedUInt8 :: Bounded UInt8 where
  top = UInt8 255
  bottom = UInt8 0

instance ordUInt8 :: Ord UInt8 where
  compare (UInt8 a) (UInt8 b) = compare a b

instance eqUInt8 :: Eq UInt8 where
  eq (UInt8 a) (UInt8 b) = eq a b

instance boundedOrdUInt8 :: BoundedOrd UInt8 where

instance arbitraryUInt8 :: Arbitrary UInt8 where
  arbitrary = intToByte <$> arbitrary

instance booleanAlgrebraUInt8 :: BooleanAlgebra UInt8 where
  conj a b = intToByte $ (byteToInt a) .&. (byteToInt b)
  disj a b = intToByte $ (byteToInt a) .|. (byteToInt b)
  not = intToByte <<< complement <<< byteToInt

instance integralUInt8 :: Integral UInt8 where
  toBigInt = BigInt.fromInt <<< byteToInt
  fromBigInt x = intToByte <<< floor <<< BigInt.toNumber $ x `mod` (BigInt.fromInt 256)

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

type UInt16 = LargeKey UInt8 UInt8
type UInt32 = LargeKey UInt16 UInt16
type UInt64 = LargeKey UInt32 UInt32
type UInt128 = LargeKey UInt64 UInt64
