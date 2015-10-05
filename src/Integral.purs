module Integral
  ( Integral
  , fromBigInt
  , toBigInt
  , fromIntegral
  , clamp
  ) where

import Prelude
import qualified Data.BigInt as BigInt

-- | `Integral` represents integers.
-- |
-- | In addition to satifying the laws of `BoundedOrd`, instances of `Integral` should satisfy the following law:
-- |
-- | `fromBigInt (toBigInt x) == x`
class (BoundedOrd a) <= Integral a where
  toBigInt :: a -> BigInt.BigInt
  fromBigInt :: BigInt.BigInt -> a

fromIntegral :: forall a b. (Integral a, Integral b) => a -> b
fromIntegral = fromBigInt <<< toBigInt

clamp :: forall b. (Integral b) => BigInt.BigInt -> b
clamp x = fromBigInt (if x > limit then limit else x) where
  limit = toBigInt (top :: b)
