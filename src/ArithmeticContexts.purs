module ArithmeticContexts
  ( SaturatingArithmetic(..)
  , runSat
  ) where

import Prelude
import qualified Data.BigInt as BigInt
import Bytes (Bytes, fromBigInt, toBigInt, clamp)

newtype SaturatingArithmetic a = SaturatingArithmetic a

runSat :: forall a. SaturatingArithmetic a -> a
runSat (SaturatingArithmetic a) = a

instance functorSaturatingArithmetic :: Functor SaturatingArithmetic where
  map f (SaturatingArithmetic a) = SaturatingArithmetic (f a)

instance applySaturatingArithmetic :: Apply SaturatingArithmetic where
  apply (SaturatingArithmetic f) (SaturatingArithmetic x) = SaturatingArithmetic (f x)

instance applicativeSaturatingArithmetic :: Applicative SaturatingArithmetic where
  pure = SaturatingArithmetic


instance semiringSaturatingArithmeticBytes :: (Bytes a) => Semiring (SaturatingArithmetic a) where
  add x y = clamp <$> ((+) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  zero = pure <<< fromBigInt $ zero :: BigInt.BigInt
  mul x y = clamp <$> ((*) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  one = pure <<< fromBigInt $ one :: BigInt.BigInt
