module LargeKey
  ( LargeKey(..)
  ) where

import Prelude
import qualified Data.BigInt as BigInt
import qualified Data.BigInt.Bits as BigIntBits
import Bytes (Bytes, toBigInt, fromBigInt)

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

instance booleanAlgebraLargeKey :: (Bytes a, Bytes b, BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (LargeKey a b) where
  conj x y = fromBigInt $ BigIntBits.(.&.) (toBigInt x) (toBigInt y)
  disj x y = fromBigInt $ BigIntBits.(.|.) (toBigInt x) (toBigInt y)
  not = fromBigInt <<< BigIntBits.complement <<< toBigInt

instance bytesLargeKey :: (Bytes a, Bytes b) => Bytes (LargeKey a b) where
  toBigInt (LargeKey x y) = hi + lo where
    shiftAmount = toBigInt (top :: b) + (one :: BigInt.BigInt)
    hi = shiftAmount * (toBigInt x)
    lo = toBigInt y
  fromBigInt x = (LargeKey hi lo) where
    shiftAmount = toBigInt (top :: b) + (one:: BigInt.BigInt)
    hi = fromBigInt $ x `div` shiftAmount
    lo = fromBigInt $ x `mod` shiftAmount
