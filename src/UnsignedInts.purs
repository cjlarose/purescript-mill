module UnsignedInts
  ( UInt8()
  , UInt16()
  , UInt32()
  , UInt64()
  , clamp
  , intToByte
  , byteToInt
  ) where

import Prelude
import LargeKey (LargeKey(..))

data UInt8 = UInt8 Int

byteToInt :: UInt8 -> Int
byteToInt (UInt8 x) = x

intToByte :: Int -> UInt8
intToByte x = UInt8 (x `mod` 256)

clamp :: Int -> UInt8
clamp x = UInt8 val where
  val = if x > 255 then 255 else x

instance uInt8Show :: Show UInt8 where
  show (UInt8 x) = "UInt8 " ++ show x

instance uInt8Bounded :: Bounded UInt8 where
  top = UInt8 255
  bottom = UInt8 0

instance uInt8Ord :: Ord UInt8 where
  compare (UInt8 a) (UInt8 b) = compare a b

instance uInt8Eq :: Eq UInt8 where
  eq (UInt8 a) (UInt8 b) = eq a b

instance uInt8BoundedOrd :: BoundedOrd UInt8 where

type UInt16 = LargeKey UInt8 UInt8
type UInt32 = LargeKey UInt16 UInt16
type UInt64 = LargeKey UInt32 UInt32
