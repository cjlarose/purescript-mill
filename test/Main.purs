module Test.Main where

import Control.Monad.Eff.Console
import Test.QuickCheck (quickCheck, (===))
import Prelude
import ArithmeticContexts (ModularArithmetic(..), SaturatingArithmetic(..))
import UnsignedInts (intToByte, UInt8())

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

main = do
  modularArithmetic
  saturatingArithmetic
