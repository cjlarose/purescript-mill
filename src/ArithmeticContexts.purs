module ArithmeticContexts
  ( ModularArithmetic(..)
  , runMod
  , SaturatingArithmetic(..)
  , runSat
  ) where

import Prelude
import UnsignedInts (UInt8(..), clamp, intToByte)
import Bytes (Bytes, fromBigInt, bytesToBigInt)
import LargeKey (LargeKey(..))

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

instance modularArithmetricLargeKeySemiring :: (Bytes a, Bytes b, Semiring (ModularArithmetic a), Semiring (ModularArithmetic b)) => Semiring (ModularArithmetic (LargeKey a b)) where
  add (ModularArithmetic x) (ModularArithmetic y) = ModularArithmetic <<< fromBigInt $ (bytesToBigInt x) + (bytesToBigInt y)
  zero = ModularArithmetic (LargeKey (runMod (zero :: ModularArithmetic a)) (runMod (zero :: ModularArithmetic b)))
  mul (ModularArithmetic x) (ModularArithmetic y) = ModularArithmetic <<< fromBigInt $ (bytesToBigInt x) * (bytesToBigInt y)
  one = ModularArithmetic (LargeKey (runMod (zero :: ModularArithmetic a)) (runMod (one :: ModularArithmetic b)))
