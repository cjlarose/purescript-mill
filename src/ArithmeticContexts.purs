module ArithmeticContexts
  ( ModularArithmetic(..)
  , runMod
  , SaturatingArithmetic(..)
  , runSat
  ) where

import Prelude
import qualified Data.BigInt as BigInt
import UnsignedInts (UInt8(), intToByte, byteToInt)
import Bytes (Bytes, fromBigInt, toBigInt, clamp)
import LargeKey (LargeKey(..))

newtype ModularArithmetic a = ModularArithmetic a

runMod :: forall a. ModularArithmetic a -> a
runMod (ModularArithmetic a) = a

instance functorModularArithmetic :: Functor ModularArithmetic where
  map f (ModularArithmetic a) = ModularArithmetic (f a)

instance applyModularArithmetic :: Apply ModularArithmetic where
  apply (ModularArithmetic f) (ModularArithmetic x) = ModularArithmetic (f x)

instance applicativeModularArithmetic :: Applicative ModularArithmetic where
  pure = ModularArithmetic


newtype SaturatingArithmetic a = SaturatingArithmetic a

runSat :: forall a. SaturatingArithmetic a -> a
runSat (SaturatingArithmetic a) = a

instance functorSaturatingArithmetic :: Functor SaturatingArithmetic where
  map f (SaturatingArithmetic a) = SaturatingArithmetic (f a)

instance applySaturatingArithmetic :: Apply SaturatingArithmetic where
  apply (SaturatingArithmetic f) (SaturatingArithmetic x) = SaturatingArithmetic (f x)

instance applicativeSaturatingArithmetic :: Applicative SaturatingArithmetic where
  pure = SaturatingArithmetic


instance semiringModularArithmeticUInt8 :: Semiring (ModularArithmetic UInt8) where
  add a b = intToByte <$> ((+) <$> (byteToInt <$> a) <*> (byteToInt <$> b))
  zero = pure <<< intToByte $ 0
  mul a b = intToByte <$> ((*) <$> (byteToInt <$> a) <*> (byteToInt <$> b))
  one = pure <<< intToByte $ 1

instance semiringSaturatingArithmeticUInt8 :: Semiring (SaturatingArithmetic UInt8) where
  add a b = clamp <<< BigInt.fromInt <$> ((+) <$> (byteToInt <$> a) <*> (byteToInt <$> b))
  zero = pure <<< intToByte $ 0
  mul a b = clamp <<< BigInt.fromInt <$> ((*) <$> (byteToInt <$> a) <*> (byteToInt <$> b))
  one = pure <<< intToByte $ 1

instance semiringModularArithmeticLargeKey :: (Bytes a, Bytes b, Semiring (ModularArithmetic a), Semiring (ModularArithmetic b)) => Semiring (ModularArithmetic (LargeKey a b)) where
  add x y = fromBigInt <$> ((+) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  zero = pure <<< fromBigInt $ zero :: BigInt.BigInt
  mul x y = fromBigInt <$> ((*) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  one = pure <<< fromBigInt $ one :: BigInt.BigInt

instance semiringSaturatingArithmeticLargeKey :: (Bytes a, Bytes b, Semiring (SaturatingArithmetic a), Semiring (SaturatingArithmetic b)) => Semiring (SaturatingArithmetic (LargeKey a b)) where
  add x y = clamp <$> ((+) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  zero = pure <<< fromBigInt $ zero :: BigInt.BigInt
  mul x y = clamp <$> ((*) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  one = pure <<< fromBigInt $ one :: BigInt.BigInt
