module ArithmeticContexts
  ( ModularArithmetic(..)
  , runMod
  , SaturatingArithmetic(..)
  , runSat
  ) where

import Prelude
import UnsignedInts (UInt8(), clamp, intToByte, byteToInt)
import Bytes (Bytes, fromBigInt, bytesToBigInt)
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
  add a b = clamp <$> ((+) <$> (byteToInt <$> a) <*> (byteToInt <$> b))
  zero = pure <<< clamp $ 0
  mul a b = clamp <$> ((*) <$> (byteToInt <$> a) <*> (byteToInt <$> b))
  one = pure <<< clamp $ 1

instance semiringModularArithmeticLargeKey :: (Bytes a, Bytes b, Semiring (ModularArithmetic a), Semiring (ModularArithmetic b)) => Semiring (ModularArithmetic (LargeKey a b)) where
  add x y = fromBigInt <$> ((+) <$> (bytesToBigInt <$> x) <*> (bytesToBigInt <$> y))
  zero = pure $ LargeKey (runMod (zero :: ModularArithmetic a)) (runMod (zero :: ModularArithmetic b))
  mul x y = fromBigInt <$> ((*) <$> (bytesToBigInt <$> x) <*> (bytesToBigInt <$> y))
  one = pure $ LargeKey (runMod (zero :: ModularArithmetic a)) (runMod (one :: ModularArithmetic b))
