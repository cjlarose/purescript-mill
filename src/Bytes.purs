module Bytes
  ( Bytes
  , fromBigInt
  , toBigInt
  , fromBytes
  , clamp
  ) where

import Prelude
import qualified Data.BigInt as BigInt

class (BoundedOrd a) <= Bytes a where
  toBigInt :: a -> BigInt.BigInt
  fromBigInt :: BigInt.BigInt -> a

fromBytes :: forall a b. (Bytes a, Bytes b) => a -> b
fromBytes = fromBigInt <<< toBigInt

clamp :: forall b. (Bytes b) => BigInt.BigInt -> b
clamp x = fromBigInt (if x > limit then limit else x) where
  limit = toBigInt (top :: b)
