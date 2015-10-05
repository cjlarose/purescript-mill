module ArithmeticContexts
  ( ModularArithmetic(..)
  , runMod
  , SaturatingArithmetic(..)
  , runSat
  ) where

import Prelude
import qualified Data.BigInt as BigInt
import Integral (Integral, fromBigInt, toBigInt, clamp)
import Test.QuickCheck.Arbitrary (arbitrary, Arbitrary)

newtype ModularArithmetic a = ModularArithmetic a

runMod :: forall a. ModularArithmetic a -> a
runMod (ModularArithmetic a) = a

instance eqModularArithmetic :: (Eq a) => Eq (ModularArithmetic  a) where
  eq (ModularArithmetic x) (ModularArithmetic y) = eq x y

instance showModularArithmetic :: (Show a) => Show (ModularArithmetic a) where
  show (ModularArithmetic x) = "ModularArithmetic " ++ show x

instance functorModularArithmetic :: Functor ModularArithmetic where
  map f (ModularArithmetic a) = ModularArithmetic (f a)

instance applyModularArithmetic :: Apply ModularArithmetic where
  apply (ModularArithmetic f) (ModularArithmetic x) = ModularArithmetic (f x)

instance applicativeModularArithmetic :: Applicative ModularArithmetic where
  pure = ModularArithmetic

instance arbitraryModularArithmetic :: (Arbitrary a) => Arbitrary (ModularArithmetic a) where
  arbitrary = pure <$> arbitrary


newtype SaturatingArithmetic a = SaturatingArithmetic a

runSat :: forall a. SaturatingArithmetic a -> a
runSat (SaturatingArithmetic a) = a

instance eqSaturatingArithmetic :: (Eq a) => Eq (SaturatingArithmetic  a) where
  eq (SaturatingArithmetic x) (SaturatingArithmetic y) = eq x y

instance showSaturatingArithmetic :: (Show a) => Show (SaturatingArithmetic a) where
  show (SaturatingArithmetic x) = "SaturatingArithmetic " ++ show x

instance functorSaturatingArithmetic :: Functor SaturatingArithmetic where
  map f (SaturatingArithmetic a) = SaturatingArithmetic (f a)

instance applySaturatingArithmetic :: Apply SaturatingArithmetic where
  apply (SaturatingArithmetic f) (SaturatingArithmetic x) = SaturatingArithmetic (f x)

instance applicativeSaturatingArithmetic :: Applicative SaturatingArithmetic where
  pure = SaturatingArithmetic

instance arbitrarySaturatingArithmetic:: (Arbitrary a) => Arbitrary (SaturatingArithmetic a) where
  arbitrary = pure <$> arbitrary


instance semiringModularArithmetic :: (Integral a) => Semiring (ModularArithmetic a) where
  add x y = fromBigInt <$> ((+) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  zero = pure <<< fromBigInt $ zero :: BigInt.BigInt
  mul x y = fromBigInt <$> ((*) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  one = pure <<< fromBigInt $ one :: BigInt.BigInt

instance semiringSaturatingArithmetic :: (Integral a) => Semiring (SaturatingArithmetic a) where
  add x y = clamp <$> ((+) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  zero = pure <<< fromBigInt $ zero :: BigInt.BigInt
  mul x y = clamp <$> ((*) <$> (toBigInt <$> x) <*> (toBigInt <$> y))
  one = pure <<< fromBigInt $ one :: BigInt.BigInt
