{-# LANGUAGE RecordWildCards #-}

module Box where


import Prelude hiding ((<>))
import Text.PrettyPrint.Boxes hiding (columns)

import Data.Bits (Bits())
import Data.Bits.Pretty
import Data.List

import Data.SVD.Types
import Data.SVD.Pretty

fmtColumn :: [String] -> Box
fmtColumn items =
     vSepDeco
  // punctuateV center2 vSepDeco (map (text) items)
  // vSepDeco
  where width = maximum $ map length items
        vSepDeco = text ( replicate width '-' )

table :: [[String]] -> Box
table rows = hSepDeco <> punctuateH top hSepDeco (map fmtColumn columns) <> hSepDeco
   where
     columns = transpose rows
     nrows = length rows
     hSepDeco =  vcat left $ map char ("+" ++ (concat $ replicate nrows "|+"))

-- Render fields as table using boxes
-- If table would be too wide split it into two tables
renderFields :: (Bits a, Num a, Show a, Integral a) => [(a, Field)] -> IO () -- Box
renderFields fs | headerSize >= 80 = do
  putStrLn "MSB"
  putStrLn $ render $ table $ remap $ takeBits 16 fs
  putStrLn "LSB"
  putStrLn $ render $ table $ remap $ dropBits 16 fs
  where headerSize = sum $ map (length . showField . snd) fs
renderFields fs | otherwise = putStrLn . render . table . remap $ fs

remap fs = [ map (showField . snd) fs
           , map (\(v, f) -> hexFieldVal f v) fs ]

takeBits 0 _ = []
takeBits x (y@(_, f):fs) | x >= fieldBitWidth f = y : (takeBits (x - fieldBitWidth f) fs)
takeBits x (y@(_, f):fs) | x <  fieldBitWidth f = [splitField x y]
  where
    splitField x (v, f) = (v, f { fieldBitWidth = x, fieldBitOffset = fieldBitOffset f + (fieldBitWidth f - x) })

dropBits 0 fs = fs
dropBits x (y@(_, f):fs) | x >= fieldBitWidth f = dropBits (x - fieldBitWidth f) fs
dropBits x (y@(_, f):fs) | x <  fieldBitWidth f = (splitField x y):fs
  where
    splitField x (v, f) = (v, f { fieldBitWidth = fieldBitWidth f - x })
