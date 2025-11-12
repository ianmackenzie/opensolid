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
import OpenSolid.Prelude
import Prelude qualified

bytes :: Builder -> ByteString
bytes builder = ByteString.toStrict (Builder.toLazyByteString builder)

empty :: Builder
empty = Prelude.mempty

concat :: List Builder -> Builder
concat = Prelude.mconcat

combine :: Foldable list => (a -> Builder) -> list a -> Builder
combine = Prelude.foldMap

{-# INLINE uint8 #-}
uint8 :: Int -> Builder
uint8 = Builder.word8 . fromIntegral

{-# INLINE uint16LE #-}
uint16LE :: Int -> Builder
uint16LE = Builder.word16LE . fromIntegral

{-# INLINE uint32LE #-}
uint32LE :: Int -> Builder
uint32LE = Builder.word32LE . fromIntegral

{-# INLINE uint64LE #-}
uint64LE :: Int -> Builder
uint64LE = Builder.word64LE . fromIntegral

{-# INLINE int16LE #-}
int16LE :: Int -> Builder
int16LE = Builder.int16LE . fromIntegral

{-# INLINE int32LE #-}
int32LE :: Int -> Builder
int32LE = Builder.int32LE . fromIntegral

{-# INLINE int64LE #-}
int64LE :: Int -> Builder
int64LE = Builder.int64LE . fromIntegral

{-# INLINE float32LE #-}
float32LE :: Number -> Builder
float32LE (Quantity double) = Builder.floatLE (GHC.Float.double2Float double)

{-# INLINE float64LE #-}
float64LE :: Number -> Builder
float64LE (Quantity double) = Builder.doubleLE double
