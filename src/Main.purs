module Main where

import Control.Monad.Eff.Console
import Prelude
import Data.Foldable (foldr)
import Data.List
import Data.Tuple

data UInt8 = UInt8 (Array Int)
data UInt16 = UInt16 (Array Int)
data UInt32 = UInt32 (Array Int)
data UInt64 = UInt64 (Array Int)
data UInt128 = UInt128 (Array Int)

instance showUInt64 :: Show UInt64 where
  show (UInt64 bs) = show bs

data BeltElement = NaR UInt64 | Result UInt64

instance showBeltElement :: Show BeltElement where
  show (NaR x) = "NaR " ++ show x
  show (Result x) = "Result " ++ show x

addu :: List Int -> List Int -> List Int
addu xs ys = (foldr f Nil (zip xs ys)) where
  f (Tuple x y) Nil = ((x + y) / 256) : ((x + y) `mod` 256) : Nil
  f (Tuple x y) (Cons prevCarry rest) = let sum = x + y + prevCarry in
    (sum / 256) : (sum `mod` 256) : rest

main = do
  -- log (show $ addu 5 6)
  log (show $ addu (toList [1, 2, 3]) (toList [7, 8, 9]))
