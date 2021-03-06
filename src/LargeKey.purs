module LargeKey
  ( LargeKey(..)
  ) where

import Prelude
import qualified Data.BigInt as BigInt
import qualified Data.BigInt.Bits as BigIntBits
import Integral (Integral, toBigInt, fromBigInt)
import Bits (Bits, popCount)

data LargeKey a b = LargeKey a b

instance eqLargeKey :: (Eq a, Eq b) => Eq (LargeKey a b) where
  eq (LargeKey a b) (LargeKey c d) = a == c && b == d

instance boundedLargeKey :: (Bounded a, Bounded b) => Bounded (LargeKey a b) where
  top = LargeKey top top
  bottom = LargeKey bottom bottom

instance showLargeKey :: (Show a, Show b) => Show (LargeKey a b) where
  show (LargeKey a b) = "LargeKey (" ++ show a ++ ") (" ++ show b ++ ")"

instance ordLargeKey :: (Ord a, Ord b) => Ord (LargeKey a b) where
  compare (LargeKey a b) (LargeKey c d) = case compare a c of
                                           LT -> LT
                                           GT -> GT
                                           EQ -> compare b d

instance boundedOrdLargeKey :: (BoundedOrd a, BoundedOrd b) => BoundedOrd (LargeKey a b) where

instance booleanAlgebraLargeKey :: (Integral a, Integral b, BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (LargeKey a b) where
  conj x y = fromBigInt $ BigIntBits.(.&.) (toBigInt x) (toBigInt y)
  disj x y = fromBigInt $ BigIntBits.(.|.) (toBigInt x) (toBigInt y)
  not = fromBigInt <<< BigIntBits.complement <<< toBigInt

instance integralLargeKey :: (Integral a, Integral b) => Integral (LargeKey a b) where
  toBigInt (LargeKey x y) = hi + lo where
    shiftAmount = toBigInt (top :: b) + (one :: BigInt.BigInt)
    hi = shiftAmount * (toBigInt x)
    lo = toBigInt y
  fromBigInt x = (LargeKey hi lo) where
    shiftAmount = toBigInt (top :: b) + (one:: BigInt.BigInt)
    hi = fromBigInt $ x `div` shiftAmount
    lo = fromBigInt $ x `mod` shiftAmount

instance bitsLargeKey :: (Bits a, Bits b, Integral a, Integral b, Integral (LargeKey a b)) => Bits (LargeKey a b) where
  xor x y = fromBigInt $ BigIntBits.(.^.) (toBigInt x) (toBigInt y)
  shift x n = fromBigInt $ BigIntBits.shiftRight (toBigInt x) n
  rotate x n = fromBigInt masked where
    bigx = (toBigInt x)
    mask = toBigInt $ top `asTypeOf` x
    shiftAmount = mask + (one :: BigInt.BigInt)
    repeated = BigIntBits.(.|.) (shiftAmount * bigx) bigx
    shifted = BigIntBits.shiftRight repeated n
    masked = BigIntBits.(.&.) shifted mask
  testBit x n = (shifted BigIntBits..&. one) == one where
    one = BigInt.fromInt 1
    shifted = BigIntBits.shiftRight (toBigInt x) n
  popCount (LargeKey a b) = popCount a + popCount b
