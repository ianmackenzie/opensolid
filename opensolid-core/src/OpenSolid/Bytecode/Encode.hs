module OpenSolid.Bytecode.Encode (word, int, float, list) where

import Data.ByteString.Builder qualified as Builder
import Data.Word (Word16)
import GHC.ByteOrder qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Prelude (Double)

word :: Word16 -> Builder
word = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.word16LE
  GHC.ByteOrder.BigEndian -> Builder.word16BE

int :: Int -> Builder
int value
  | value >= 0 && value < 65536 = word (fromIntegral value)
  | otherwise =
      internalError ("Bytecode only supports integers in 0-65535 range, got " <> Text.int value)

double :: Double -> Builder
double = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.doubleLE
  GHC.ByteOrder.BigEndian -> Builder.doubleBE

float :: Float -> Builder
float = double . Float.toDouble

list :: (a -> Builder) -> List a -> Builder
list encodeItem items = int items.length <> Binary.collect encodeItem items
