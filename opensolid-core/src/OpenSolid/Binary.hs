module OpenSolid.Binary
  ( ByteString
  , Builder
  , bytes
  , empty
  , concat
  , combine
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
import OpenSolid.Int qualified as Int
import OpenSolid.Prelude hiding (concat)
import OpenSolid.Quantity (Quantity (Quantity))

bytes :: Builder -> ByteString
bytes builder = ByteString.toStrict (Builder.toLazyByteString builder)

empty :: Builder
empty = mempty

concat :: List Builder -> Builder
concat = mconcat

combine :: Foldable list => (a -> Builder) -> list a -> Builder
combine = foldMap

{-# INLINE uint8 #-}
uint8 :: Int -> Builder
uint8 = Builder.word8 . Int.toWord8

{-# INLINE uint16LE #-}
uint16LE :: Int -> Builder
uint16LE = Builder.word16LE . Int.toWord16

{-# INLINE uint32LE #-}
uint32LE :: Int -> Builder
uint32LE = Builder.word32LE . Int.toWord32

{-# INLINE uint64LE #-}
uint64LE :: Int -> Builder
uint64LE = Builder.word64LE . Int.toWord64

{-# INLINE int16LE #-}
int16LE :: Int -> Builder
int16LE = Builder.int16LE . Int.toInt16

{-# INLINE int32LE #-}
int32LE :: Int -> Builder
int32LE = Builder.int32LE . Int.toInt32

{-# INLINE int64LE #-}
int64LE :: Int -> Builder
int64LE = Builder.int64LE . Int.toInt64

{-# INLINE float32LE #-}
float32LE :: Number -> Builder
float32LE (Quantity double) = Builder.floatLE (GHC.Float.double2Float double)

{-# INLINE float64LE #-}
float64LE :: Number -> Builder
float64LE (Quantity double) = Builder.doubleLE double
