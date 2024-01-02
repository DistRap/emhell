{-# LANGUAGE MultiWayIf #-}
module EmHell.SVD.Manipulation
  ( setField
  ) where

import Control.Lens ((^.), view)
import Data.Bits (Bits, (.|.), shiftL)
import Data.SVD
import Text.Printf (PrintfArg)
import qualified Data.Bits.Pretty
import qualified Data.Char

-- | Set @Field@ of a @Register@ to new value
-- and return new value of a whole register
-- or Left if field is not found or value
-- is too large to fit
setField
  :: ( Integral a
     , Bits a
     , PrintfArg a
     , Show a
     )
  => Register
  -> a -- ^ Old register value
  -> String -- ^ Field name
  -> a -- ^ Field value
  -> Either String a
setField r oldRegVal fName v =
  case filter ((==fName) . (map Data.Char.toLower) . view name) (r ^. fields) of
    [f] ->
      let maxVal = fromIntegral ((2 :: Int) ^ (f ^. bitWidth) - 1)
      in
        if | v > maxVal ->
              Left
                $  "Value too large for field "
                <> f ^. name
                <> " with bit width of "
                <> show (f ^. bitWidth)
                <> "\n"
                <> "HEX "
                <> Data.Bits.Pretty.formatHex v
                <> " ∉ [0.."
                <> Data.Bits.Pretty.formatHex maxVal
                <> "]"
                <> "\n"
                <> "DEC "
                <> show v
                <> " ∉ [0.."
                <> show maxVal
                <> "]"
           | r ^. access == ReadOnly ->
              Left
                $ "Register is read-only"
           | otherwise ->
              pure $ oldRegVal .|. (v `shiftL` (f ^. bitOffset))
    _ -> Left "Field not found"
