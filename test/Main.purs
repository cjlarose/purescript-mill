module Test.Main where

import Control.Monad.Eff.Console
import Test.QuickCheck (quickCheck, (===))
import Prelude
import ArithmeticContexts (ModularArithmetic(..))
import UnsignedInts (intToByte)

main = do
  quickCheck \n -> n + (ModularArithmetic (intToByte 1)) === (ModularArithmetic (intToByte 1)) + n
