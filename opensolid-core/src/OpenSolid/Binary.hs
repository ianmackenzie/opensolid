module OpenSolid.Binary
  ( ByteString
  , Builder
  , bytes
  , empty
  , concat
  , collect
  , uint8
  , uint16LE
  , uint32LE
  , uint64LE
  , int16LE
  , int32LE
  , int64LE
  , float32LE
  , float64LE
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import GHC.Float qualified
import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.Int qualified as Int
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty))
import Prelude qualified

bytes :: Builder -> ByteString
bytes = Builder.toLazyByteString >> ByteString.toStrict

empty :: Builder
empty = Prelude.mempty

concat :: List Builder -> Builder
concat = Prelude.mconcat

collect :: Foldable list => (a -> Builder) -> list a -> Builder
collect = Prelude.foldMap

uint8 :: Int -> Builder
uint8 = Builder.word8 . Int.toWord8

uint16LE :: Int -> Builder
uint16LE = Builder.word16LE . Int.toWord16

uint32LE :: Int -> Builder
uint32LE = Builder.word32LE . Int.toWord32

uint64LE :: Int -> Builder
uint64LE = Builder.word64LE . Int.toWord64

int16LE :: Int -> Builder
int16LE = Builder.int16LE . Int.toInt16

int32LE :: Int -> Builder
int32LE = Builder.int32LE . Int.toInt32

int64LE :: Int -> Builder
int64LE = Builder.int64LE . Int.toInt64

float32LE :: Float -> Builder
float32LE (Qty double) = Builder.floatLE (GHC.Float.double2Float double)

float64LE :: Float -> Builder
float64LE (Qty double) = Builder.doubleLE double
