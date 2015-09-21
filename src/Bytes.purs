module Bytes
  ( Bytes
  , fromBigInt
  , toBytes
  , bytesToBigInt
  , clamp
  ) where

import Prelude
import Data.Int (floor)
import Data.List
import Data.Foldable (foldl)
import qualified Data.BigInt as BigInt
import UnsignedInts (UInt8(), intToByte, byteToInt)
import LargeKey (LargeKey(..))

class (BoundedOrd a) <= Bytes a where
  toBytes :: a -> List UInt8
  fromBigInt :: BigInt.BigInt -> a

bytesToBigInt :: forall b. (Bytes b) => b -> BigInt.BigInt
bytesToBigInt x = foldl f (BigInt.fromInt 0) (toBytes x) where
  shiftL8 = (*) (BigInt.fromInt 256)
  f acc byte = (shiftL8 acc) + (BigInt.fromInt <<< byteToInt $ byte)

clamp :: forall b. (Bytes b) => BigInt.BigInt -> b
clamp x = fromBigInt (if x > limit then limit else x) where
  limit = bytesToBigInt (top :: b)

instance bytesUInt8 :: Bytes UInt8 where
  toBytes = pure
  fromBigInt x = intToByte <<< floor <<< BigInt.toNumber $ x `mod` (BigInt.fromInt 256)

instance bytesLargeKey :: (Bytes a, Bytes b) => Bytes (LargeKey a b) where
  toBytes (LargeKey a b) = (toBytes a) ++ (toBytes b)
  fromBigInt x = (LargeKey hi lo) where
    shiftAmount = bytesToBigInt (top :: b) + BigInt.fromInt 1
    hi = fromBigInt $ x `div` shiftAmount
    lo = fromBigInt $ x `mod` shiftAmount
