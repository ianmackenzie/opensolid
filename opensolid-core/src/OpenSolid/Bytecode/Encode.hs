module OpenSolid.Bytecode.Encode (word, int, number, list) where

import Data.ByteString.Builder qualified as Builder
import Data.Word (Word16)
import GHC.ByteOrder qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

word :: Word16 -> Builder
word = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.word16LE
  GHC.ByteOrder.BigEndian -> Builder.word16BE

int :: Int -> Builder
int value
  | value >= 0 && value < 65536 = word (fromIntegral value)
  | otherwise = abort ("Bytecode only supports integers in 0-65535 range, got " <> Text.int value)

double :: Double -> Builder
double = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.doubleLE
  GHC.ByteOrder.BigEndian -> Builder.doubleBE

number :: Number -> Builder
number (Quantity value) = double value

list :: (a -> Builder) -> List a -> Builder
list encodeItem items = int (List.length items) <> Binary.combine encodeItem items
