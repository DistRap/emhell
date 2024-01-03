{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module EmHell.SVD.Manipulation
  ( setField
  ) where

import Control.Lens ((^.), view)
import Data.Bits (Bits, (.|.), (.&.), complement, shiftL)
import Data.SVD
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), bold, color)
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
     , Pretty a
     )
  => Register
  -> a -- ^ Old register value
  -> String -- ^ Field name
  -> a -- ^ Field value
  -> Either (Doc AnsiStyle) a
setField r oldRegVal fName v =
  case filter ((==fName) . (map Data.Char.toLower) . view name) (r ^. fields) of
    [f] ->
      let maxVal = fromIntegral ((2 :: Int) ^ (f ^. bitWidth) - 1)
      in
        if | v > maxVal ->
              Left $ vsep
                [ annotate
                    (color Red)
                    (   "Value too large for field "
                     <> annotate
                          bold
                          (pretty (f ^. name))
                     <> " with bit width of "
                     <> annotate
                          bold
                          (pretty (f ^. bitWidth))
                    )
                , annotate
                    (color Cyan)
                    (   "HEX "
                     <> pretty (Data.Bits.Pretty.formatHex v)
                     <> " ∉ [0.."
                     <> pretty (Data.Bits.Pretty.formatHex maxVal)
                     <> "]"
                    )
                , annotate
                    (color Green)
                    (   "DEC "
                     <> pretty v
                     <> " ∉ [0.."
                     <> pretty maxVal
                     <> "]"
                    )
                ]
           | r ^. access `elem` [ReadOnly, ReadWriteOnce, WriteOnce] ->
              Left
                $ annotate
                    (color Red)
                    (   "Register is"
                    <+> pretty (showAccessType (r ^. access))
                    )
           | f ^. reserved ->
              Left
                $ annotate (color Red) "Field is reserved"
           | otherwise ->
              pure $
                let pos = f ^. bitOffset
                    val = v `shiftL` pos
                    fmax = 2 ^ (f ^. bitWidth) - 1
                    mask = complement (fmax `shiftL` pos)
                in oldRegVal .&. mask .|. val
    _ -> Left "Field not found"
