module Main where

import Control.Monad.Eff.Console
import Prelude
import Data.Foldable (foldr)
import Data.List
import Data.Tuple

data UInt8 = UInt8 Int

instance uInt8Show :: Show UInt8 where
  show (UInt8 x) = "UInt8 " ++ show x

instance uInt8Bounded :: Bounded UInt8 where
  top = UInt8 255
  bottom = UInt8 0

fromInt :: Int -> UInt8
fromInt x = UInt8 (x `mod` 256)

data LargeKey a b = LargeKey a b

hiHalf :: forall a b. LargeKey a b -> a
hiHalf (LargeKey a _) = a

loHalf :: forall a b. LargeKey a b -> b
loHalf (LargeKey _ b) = b

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

type UInt16 = LargeKey UInt8 UInt8
type UInt32 = LargeKey UInt16 UInt16

-- addLists :: List Int -> List Int -> (Tuple Boolean (List Int))
-- addLists xs ys = (foldr f (Tuple false Nil) (zip xs ys)) where
--   f (Tuple x y) (Tuple carry prevSum) = (Tuple newCarry newSum) where
--     c = (if carry then 1 else 0)
--     sum = x + y + c
--     newSum = (sum `mod` 256) : prevSum
--     newCarry = (sum / 256) == 1
  

-- addWithCarry :: forall b. (Bytes b) => b -> b -> (Tuple Boolean (List Int))
-- addWithCarry a b = addLists (toBytes a) (toBytes b)

-- modulo
-- saturating
-- excepting
-- widening

main = do
  -- log (show $ addu 5 6)
  -- log (show $ addLists (toList [1, 2, 3]) (toList [7, 8, 9]))
  log "hello world"
  log (show $ fromInt 400)
  log (show $ (top :: UInt8))
  log (show $ (top :: UInt16))
  log (show $ (top :: UInt32))
  -- log (show $ (top :: UInt32))
  -- log (show $ (top :: UInt64))
  -- log (show $ (top :: UInt128))
  -- log (show $ addu (UInt32 (toList [1, 2, 3])) (UInt32 (toList [7, 8, 9])))
  -- log (show $ addu (UInt32 (toList [1])) (UInt32 (toList [255])))
