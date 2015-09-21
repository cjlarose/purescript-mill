module LargeKey
  ( LargeKey(..)
  ) where

import Prelude

data LargeKey a b = LargeKey a b

instance largeKeyEq :: (Eq a, Eq b) => Eq (LargeKey a b) where
  eq (LargeKey a b) (LargeKey c d) = a == c && b == d

instance largeKeyBounded :: (Bounded a, Bounded b) => Bounded (LargeKey a b) where
  top = LargeKey top top
  bottom = LargeKey bottom bottom

instance largeKeyShow :: (Show a, Show b) => Show (LargeKey a b) where
  show (LargeKey a b) = "LargeKey (" ++ show a ++ ") (" ++ show b ++ ")"

instance largeKeyOrd :: (Ord a, Ord b) => Ord (LargeKey a b) where
  compare (LargeKey a b) (LargeKey c d) = case compare a c of
                                           LT -> LT
                                           GT -> GT
                                           EQ -> compare b d

instance largeKeyBoundedOrd :: (BoundedOrd a, BoundedOrd b) => BoundedOrd (LargeKey a b) where
