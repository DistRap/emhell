module EmHell.SVD.Query
  ( getRegWithAddr
  ) where

import Data.SVD (Device, Register)
import qualified Data.SVD.Util

-- | Get @Register@ and its address
-- based on peripheral name and device
getRegWithAddr
  :: String -- ^ Peripheral name
  -> String -- ^ Register name
  -> Device
  -> Either String (Register, Int)
getRegWithAddr pName rName dev =
  let
    reg =
      Data.SVD.Util.getPeriphReg
        pName
        rName
        dev
    regAddr =
      Data.SVD.Util.getPeriphRegAddr
        pName
        rName
        dev
  in
    case (reg, regAddr) of
      (Right r, Right ra) -> Right (r, ra)
      (_, _) -> Left "Register not found"
