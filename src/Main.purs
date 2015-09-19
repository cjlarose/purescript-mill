module Main where

import Control.Monad.Eff.Console
import Prelude
import Data.Foldable (foldr)
import Data.List
import Data.Tuple

-- type UInt8 = Int
data UInt8 = UInt8 Int
data UInt16 = UInt16 UInt8 UInt8
data UInt32 = UInt32 UInt16 UInt16
data UInt64 = UInt64 UInt32 UInt32
data UInt128 = UInt128 UInt64 UInt64

fromInt :: Int -> UInt8
fromInt x = UInt8 (x `mod` 256)

instance uInt8Show :: Show UInt8 where
  show (UInt8 x) = "UInt8 " ++ show x

class Bytes a where
  toBytes :: a -> List Int
  width :: Int

addWithCarry :: forall b. (Bytes b) => b -> b -> (Tuple Boolean (List Int))
addWithCarry a b = addLists (toBytes a) (toBytes b)

instance uInt8Bytes :: Bytes UInt8 where
  toBytes (UInt8 a) = pure a
  width = 1

instance uInt16Bytes :: Bytes UInt16 where
  toBytes (UInt16 a b) = (toBytes a) ++ (toBytes b)
  width = 2

instance uInt32Bytes :: Bytes UInt32 where
  toBytes (UInt32 a b) = (toBytes a) ++ (toBytes b)
  width = 4

instance uInt64Bytes :: Bytes UInt64 where
  toBytes (UInt64 a b) = (toBytes a) ++ (toBytes b)
  width = 8

instance uInt128Bytes :: Bytes UInt128 where
  toBytes (UInt128 a b) = (toBytes a) ++ (toBytes b)
  width = 16

-- data UInt = UInt8 | UInt16 | UInt32 | UInt64 | UInt128

-- addu :: forall a. (Bytes a) => a -> a -> a
-- addu a b = (toBytes a) ++ (toBytes b)

-- instance intBytes :: Bytes UInt26 where
--   toBytes (U

-- data BeltElement = NaR UInt32 | Result UInt32
-- 
-- instance showBeltElement :: Show BeltElement where
--   show (NaR x) = "NaR " ++ show x
--   show (Result x) = "Result " ++ show x
-- 

addLists :: List Int -> List Int -> (Tuple Boolean (List Int))
addLists xs ys = (foldr f (Tuple false Nil) (zip xs ys)) where
  f (Tuple x y) (Tuple carry prevSum) = (Tuple newCarry newSum) where
    c = (if carry then 1 else 0)
    sum = x + y + c
    newSum = (sum `mod` 256) : prevSum
    newCarry = (sum / 256) == 1

-- addu :: UInt32 -> UInt32 -> UInt32
-- addu (UInt32 xs) (UInt32 ys) = case addLists xs ys of
--                                     Nil -> UInt32 Nil
--                                     (Cons _ rest) -> UInt32 rest

-- modulo
-- saturating
-- excepting
-- widening

main = do
  -- log (show $ addu 5 6)
  -- log (show $ addLists (toList [1, 2, 3]) (toList [7, 8, 9]))
  log "hello world"
  log (show $ fromInt 400)
  -- log (show $ addu (UInt32 (toList [1, 2, 3])) (UInt32 (toList [7, 8, 9])))
  -- log (show $ addu (UInt32 (toList [1])) (UInt32 (toList [255])))
