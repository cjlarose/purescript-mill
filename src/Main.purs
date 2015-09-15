module Main where

import Control.Monad.Eff.Console
import Prelude

data BeltElement = NaR Int | Result Int

instance showBeltElement :: Show BeltElement where
  show (NaR x) = "NaR " ++ show x
  show (Result x) = "Result " ++ show x

addu :: Int -> Int -> BeltElement
addu x y = Result (x + y)

main = do
  log (show $ addu 5 6)
