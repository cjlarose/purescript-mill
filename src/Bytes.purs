module Bytes
  ( Bytes
  , fromBigInt
  , toBytes
  , bytesToBigInt
  ) where

import Prelude
import Data.Int (floor)
import Data.List
import Data.Foldable (foldl)
import qualified Data.BigInt as BigInt
import UnsignedInts (UInt8(..))
import LargeKey (LargeKey(..))

class (BoundedOrd a) <= Bytes a where
  toBytes :: a -> List UInt8
  fromBigInt :: BigInt.BigInt -> a

bytesToBigInt :: forall b. (Bytes b) => b -> BigInt.BigInt
bytesToBigInt x = foldl f (BigInt.fromInt 0) (toBytes x) where
  shiftL8 = (*) (BigInt.fromInt 256)
  f acc (UInt8 byte) = (shiftL8 acc) + (BigInt.fromInt byte)

instance uInt8Bytes :: Bytes UInt8 where
  toBytes = pure
  fromBigInt x = UInt8 <<< floor <<< BigInt.toNumber $ x `mod` (BigInt.fromInt 256)

instance largeKeyBytes :: (Bytes a, Bytes b) => Bytes (LargeKey a b) where
  toBytes (LargeKey a b) = (toBytes a) ++ (toBytes b)
  fromBigInt x = (LargeKey hi lo) where
    shiftAmount = bytesToBigInt (top :: b) + BigInt.fromInt 1
    hi = fromBigInt $ x `div` shiftAmount
    lo = fromBigInt $ x `mod` shiftAmount
