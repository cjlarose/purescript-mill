module Test.Main where

import Control.Monad.Eff.Console
import Test.QuickCheck (quickCheck, (===))
import Prelude
import ArithmeticContexts (ModularArithmetic(..), SaturatingArithmetic(..))
import UnsignedInts (intToByte, UInt8())
import Bits ((.^.))

(.=>.) :: forall a. (BooleanAlgebra a) => a -> a -> a
(.=>.) p q  = (not p) || q

modularArithmetic = do
  log "modular addition of UInt8 forms a communtative monoid"
  quickCheck \a b c -> (a :: ModularArithmetic UInt8 + b) + c === a + (b + c)
  quickCheck \a -> (a :: ModularArithmetic UInt8) + zero === zero + a
  quickCheck \a -> (a :: ModularArithmetic UInt8) + zero === a
  quickCheck \a b -> (a :: ModularArithmetic UInt8) + b === b + a

  log "modular multiplication of UInt8 forms a monoid"
  quickCheck \a b c -> (a :: ModularArithmetic UInt8 * b) * c === a * (b * c)
  quickCheck \a -> (a :: ModularArithmetic UInt8) * one === one * a
  quickCheck \a -> (a :: ModularArithmetic UInt8) * one === a

  log "modular mulitplication distributes over modular addition"
  quickCheck \a b c -> (a :: ModularArithmetic UInt8) * (b + c) === a * b + a * c
  quickCheck \a b c -> ((a :: ModularArithmetic UInt8) + b) * c === a * c + b * c

  log "annihilation"
  quickCheck \a -> (a :: ModularArithmetic UInt8) * zero === zero * a
  quickCheck \a -> (a :: ModularArithmetic UInt8) * zero === zero

saturatingArithmetic = do
  log "saturating addition of UInt8 forms a communtative monoid"
  quickCheck \a b c -> (a :: SaturatingArithmetic UInt8 + b) + c === a + (b + c)
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) + zero === zero + a
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) + zero === a
  quickCheck \a b -> (a :: SaturatingArithmetic UInt8) + b === b + a

  log "saturating multiplication of UInt8 forms a monoid"
  quickCheck \a b c -> (a :: SaturatingArithmetic UInt8 * b) * c === a * (b * c)
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * one === one * a
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * one === a

  log "saturating mulitplication distributes over saturating addition"
  quickCheck \a b c -> (a :: SaturatingArithmetic UInt8) * (b + c) === a * b + a * c
  quickCheck \a b c -> ((a :: SaturatingArithmetic UInt8) + b) * c === a * c + b * c

  log "annihilation"
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * zero === zero * a
  quickCheck \a -> (a :: SaturatingArithmetic UInt8) * zero === zero

eqLaws = do
  log "reflexivity"
  quickCheck \a -> (a :: UInt8) === a

  log "symmetry"
  quickCheck \a b -> eq (a :: UInt8) b === eq b a

  log "transitivity"
  quickCheck \a b c -> ((eq (a :: UInt8) b) && eq b c) .=>. (eq a c)

ordLaws = do
  log "reflexivity"
  quickCheck \a -> (a :: UInt8) <= a

  log "antisymmetry"
  quickCheck \a b -> (a :: UInt8 <= b && b <= a) .=>. a == b

  log "transitivity"
  quickCheck \a b c -> (a :: UInt8 <= b && b <= c) .=>. a <= c

boundedOrdLaws = do
  log "ordering"
  quickCheck \a -> bottom <= a :: UInt8 && a <= top

booleanAlgebraLaws = do
  log "associativity"
  quickCheck \a b c -> a :: UInt8 || (b || c) === (a || b) || c
  quickCheck \a b c -> a :: UInt8 && (b && c) === (a && b) && c

  log "commutativity"
  quickCheck \a b -> a :: UInt8 || b === b || a
  quickCheck \a b -> a :: UInt8 && b === b && a

  log "distributivity"
  quickCheck \a b c -> a :: UInt8 && (b || c) === a && b || a && c
  quickCheck \a b c -> a :: UInt8 || (b && c) === (a || b) && (a || c)

  log "idempotence"
  quickCheck \a -> a :: UInt8 || a === a
  quickCheck \a -> a :: UInt8 && a === a

  log "absorption"
  quickCheck \a b -> a :: UInt8 || (a && b) === a
  quickCheck \a b -> a :: UInt8 && (a || b) === a

  log "annihilation"
  quickCheck \a -> a :: UInt8 || top === top

  log "complementation"
  quickCheck \a -> a :: UInt8 && (not a) === bottom
  quickCheck \a -> a :: UInt8 || (not a) === top

bitsLaws = do
  log "xor law"
  quickCheck \a b -> (a :: UInt8 .^. b) === (a || b) && (not (a && b))

main = do
  eqLaws
  ordLaws
  boundedOrdLaws
  booleanAlgebraLaws
  bitsLaws
  modularArithmetic
  saturatingArithmetic
  --- integral laws tests (composition of toBigInt and fromBigInt)
