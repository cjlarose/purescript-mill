module Test.Main where

import Control.Monad.Eff.Console
import Test.QuickCheck (quickCheck, (===))
import Prelude
import ArithmeticContexts (ModularArithmetic(..), SaturatingArithmetic(..))
import UnsignedInts (intToByte, UInt8())

(.=>.) :: forall a. (BooleanAlgebra a) => a -> a -> a
(.=>.) p q  = (not p) || q

modularArithmetic = do
  -- modular addition of UInt8 forms a communtative monoid
  quickCheck \a b c -> (a :: ModularArithmetic UInt8 + b) + c === a + (b + c)
  quickCheck \a -> (a :: ModularArithmetic UInt8) + zero === zero + a
  quickCheck \a -> (a :: ModularArithmetic UInt8) + zero === a
  quickCheck \a b -> (a :: ModularArithmetic UInt8) + b === b + a
  -- modular multiplication of UInt8 forms a monoid
  quickCheck \a b c -> (a :: ModularArithmetic UInt8 * b) * c === a * (b * c)
  quickCheck \a -> (a :: ModularArithmetic UInt8) * one === one * a
  quickCheck \a -> (a :: ModularArithmetic UInt8) * one === a
  -- modular mulitplication distributes over modular addition
  quickCheck \a b c -> (a :: ModularArithmetic UInt8) * (b + c) === a * b + a * c
  quickCheck \a b c -> ((a :: ModularArithmetic UInt8) + b) * c === a * c + b * c
  -- annihilation
  quickCheck \a -> (a :: ModularArithmetic UInt8) * zero === zero * a
  quickCheck \a -> (a :: ModularArithmetic UInt8) * zero === zero

saturatingArithmetic = do
  -- saturating addition of UInt8 forms a communtative monoid
  quickCheck \a b c -> (a :: SaturatingArithmetic UInt8 + b) + c === a + (b + c)
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) + zero === zero + a
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) + zero === a
  quickCheck \a b -> (a :: SaturatingArithmetic UInt8) + b === b + a
  -- saturating multiplication of UInt8 forms a monoid
  quickCheck \a b c -> (a :: SaturatingArithmetic UInt8 * b) * c === a * (b * c)
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * one === one * a
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * one === a
  -- saturating mulitplication distributes over saturating addition
  quickCheck \a b c -> (a :: SaturatingArithmetic UInt8) * (b + c) === a * b + a * c
  quickCheck \a b c -> ((a :: SaturatingArithmetic UInt8) + b) * c === a * c + b * c
  -- annihilation
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * zero === zero * a
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * zero === zero

eqLaws = do
  -- reflexivity
  quickCheck \a -> (a :: UInt8) === a
  -- symmetry
  quickCheck \a b -> eq (a :: UInt8) b === eq b a
  -- transitivity
  quickCheck \a b c -> ((eq (a :: UInt8) b) && eq b c) .=>. (eq a c)

ordLaws = do
  -- reflexivity
  quickCheck \a -> (a :: UInt8) <= a
  -- antisymmetry
  quickCheck \a b -> (a :: UInt8 <= b && b <= a) .=>. a == b
  -- transitivity
  quickCheck \a b c -> (a :: UInt8 <= b && b <= c) .=>. a <= c

main = do
  eqLaws
  ordLaws
  modularArithmetic
  saturatingArithmetic
  --- bytes laws tests (composition of toBigInt and fromBigInt)
