module Main where

import Control.Monad.Eff.Console
import Prelude
import Data.Foldable (foldr, foldl)
import Data.List
import Data.Tuple
import Data.Int (floor)
import qualified Data.BigInt as BigInt

data UInt8 = UInt8 Int

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

newtype ModularArithmetic a = ModularArithmetic a

runMod :: forall a. ModularArithmetic a -> a
runMod (ModularArithmetic a) = a

newtype SaturatingArithmetic a = SaturatingArithmetic a

runSat :: forall a. SaturatingArithmetic a -> a
runSat (SaturatingArithmetic a) = a

instance modularArithmeticUInt8Semiring :: Semiring (ModularArithmetic UInt8) where
  add (ModularArithmetic (UInt8 a)) (ModularArithmetic (UInt8 b)) = ModularArithmetic $ intToByte $ a + b
  zero = ModularArithmetic $ intToByte 0
  mul (ModularArithmetic (UInt8 a)) (ModularArithmetic (UInt8 b)) = ModularArithmetic $ intToByte $ a * b
  one = ModularArithmetic $ intToByte 1

instance saturatingArithmeticUInt8Semiring :: Semiring (SaturatingArithmetic UInt8) where
  add (SaturatingArithmetic (UInt8 a)) (SaturatingArithmetic (UInt8 b)) = SaturatingArithmetic $ clamp $ a + b
  zero = SaturatingArithmetic $ clamp 0
  mul (SaturatingArithmetic (UInt8 a)) (SaturatingArithmetic (UInt8 b)) = SaturatingArithmetic $ clamp $ a * b
  one = SaturatingArithmetic $ clamp 1


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

data LargeKey a b = LargeKey a b

instance largeKeyEq :: (Eq a, Eq b) => Eq (LargeKey a b) where
  eq (LargeKey a b) (LargeKey c d) = a == c && b == d

instance largeKeyBounded :: (Bounded a, Bounded b) => Bounded (LargeKey a b) where
  top = LargeKey top top
  bottom = LargeKey bottom bottom

instance largeKeyShow :: (Show a, Show b) => Show (LargeKey a b) where
  show (LargeKey a b) = "LargeKey (" ++ show a ++ ") (" ++ show b ++ ")"

instance largeKeyOrd :: (Ord a, Ord b) => Ord (LargeKey a b) where
  compare (LargeKey a b) (LargeKey c d) = case compare a c of
                                           LT -> LT
                                           GT -> GT
                                           EQ -> compare b d

instance largeKeyBoundedOrd :: (BoundedOrd a, BoundedOrd b) => BoundedOrd (LargeKey a b) where

instance largeKeyBytes :: (Bytes a, Bytes b) => Bytes (LargeKey a b) where
  toBytes (LargeKey a b) = (toBytes a) ++ (toBytes b)
  fromBigInt x = (LargeKey hi lo) where
    shiftAmount = bytesToBigInt (top :: b) + BigInt.fromInt 1
    hi = fromBigInt $ x `div` shiftAmount
    lo = fromBigInt $ x `mod` shiftAmount

type UInt16 = LargeKey UInt8 UInt8
type UInt32 = LargeKey UInt16 UInt16
type UInt64 = LargeKey UInt32 UInt32

-- modulo
-- saturating
-- excepting
-- widening

main = do
  -- log (show $ addu 5 6)
  -- log (show $ addLists (toList [1, 2, 3]) (toList [7, 8, 9]))
  log "hello world"
  log (show $ intToByte 400)
  log (show $ (top :: UInt8))
  log (show $ (top :: UInt16))
  log (show $ (top :: UInt32))
  log (show $ (compare (bottom :: UInt32) (top :: UInt32)))
  log (show $ (toBytes (top :: UInt32)))
  log (show <<< runMod $ (ModularArithmetic (intToByte 200) * ModularArithmetic (intToByte 50)))
  log (show <<< runSat $ (SaturatingArithmetic (clamp 127) * SaturatingArithmetic (clamp 2)))
  log (show <<< runSat $ (SaturatingArithmetic (clamp 200) * SaturatingArithmetic (clamp 2)))
  log (show <<< bytesToBigInt $ (top :: UInt8))
  log (show <<< toBytes $ (top :: UInt16))
  log (show <<< bytesToBigInt $ (top :: UInt16))
  log (show <<< toBytes $ (top :: UInt32))
  log (show <<< bytesToBigInt $ (top :: UInt32))
  log (show <<< bytesToBigInt $ (top :: UInt32))
  log (show $ (fromBigInt <<< bytesToBigInt $ (top :: UInt16)) :: UInt32)
  -- log (show $ (top :: UInt32))
  -- log (show $ (top :: UInt64))
  -- log (show $ (top :: UInt128))
  -- log (show $ addu (UInt32 (toList [1, 2, 3])) (UInt32 (toList [7, 8, 9])))
  -- log (show $ addu (UInt32 (toList [1])) (UInt32 (toList [255])))
