module UnsignedInts
  ( UInt8()
  , UInt16()
  , UInt32()
  , UInt64()
  , intToByte
  , byteToInt
  ) where

import Prelude
import Data.Int.Bits ((.&.), (.|.), complement)
import Test.QuickCheck.Arbitrary (arbitrary, Arbitrary)
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

type UInt16 = LargeKey UInt8 UInt8
type UInt32 = LargeKey UInt16 UInt16
type UInt64 = LargeKey UInt32 UInt32
type UInt128 = LargeKey UInt64 UInt64
