module OpenSolid.Bytecode.Encode (word, int, float) where

import Data.ByteString.Builder qualified as Builder
import Data.Word (Word16)
import GHC.ByteOrder qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import Prelude (Double)

word :: Word16 -> Builder
word = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.word16LE
  GHC.ByteOrder.BigEndian -> Builder.word16BE

int :: Int -> Builder
int value
  | value < 65536 = word (fromIntegral value)
  | otherwise = exception "More than 65536 locals or constants in compiled function"

double :: Double -> Builder
double = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.doubleLE
  GHC.ByteOrder.BigEndian -> Builder.doubleBE

float :: Float -> Builder
float = double . Float.toDouble
