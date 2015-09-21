module Main where

import Control.Monad.Eff.Console
import Prelude
import qualified Data.BigInt as BigInt
import ArithmeticContexts (ModularArithmetic(..), runMod, SaturatingArithmetic(..), runSat)
import UnsignedInts (UInt8(), intToByte, UInt16(), UInt32())
import Bytes (toBigInt, fromBigInt, fromBytes)

-- modulo
-- saturating
-- excepting
-- widening

main = do
  -- log (show $ addu 5 6)
  -- log (show $ addLists (toList [1, 2, 3]) (toList [7, 8, 9]))
  log "hello world"
  log (show $ intToByte 400)
  log (show $ (top :: UInt8))
  log (show $ (top :: UInt16))
  log (show $ (top :: UInt32))
  log (show $ (compare (bottom :: UInt32) (top :: UInt32)))
  log (show $ (toBigInt (top :: UInt32)))
  log (show <<< runMod $ (ModularArithmetic (intToByte 200) * ModularArithmetic (intToByte 50)))
  log (show <<< runSat $ (SaturatingArithmetic (intToByte 240) + SaturatingArithmetic (intToByte 20)))
  log (show <<< runSat $ (SaturatingArithmetic (intToByte 127) * SaturatingArithmetic (intToByte 2)))
  log (show <<< runSat $ (SaturatingArithmetic (intToByte 200) * SaturatingArithmetic (intToByte 2)))
  log (show <<< toBigInt $ (top :: UInt8))
  log (show <<< toBigInt $ (top :: UInt16))
  log (show <<< toBigInt $ (top :: UInt32))
  log (show $ (fromBytes (top :: UInt16)) :: UInt32)
  log (show $ (fromBytes (top :: UInt32)) :: UInt16)
  log (show <<< runMod $ (ModularArithmetic ((fromBigInt (BigInt.fromInt 5)) :: UInt16)) * (ModularArithmetic (fromBigInt (BigInt.fromInt 600))))
  log (show <<< runMod $ (ModularArithmetic ((fromBigInt (BigInt.fromInt 110)) :: UInt16)) * (ModularArithmetic (fromBigInt (BigInt.fromInt 600))))
  log (show <<< runMod $ (ModularArithmetic (top :: UInt32)) + (ModularArithmetic (fromBigInt (BigInt.fromInt 2))))
  log (show <<< runSat $ (SaturatingArithmetic (top :: UInt32)) + (SaturatingArithmetic (fromBigInt (BigInt.fromInt 2))))
  -- log (show $ (top :: UInt32))
  -- log (show $ (top :: UInt64))
  -- log (show $ (top :: UInt128))
  -- log (show $ addu (UInt32 (toList [1, 2, 3])) (UInt32 (toList [7, 8, 9])))
  -- log (show $ addu (UInt32 (toList [1])) (UInt32 (toList [255])))
