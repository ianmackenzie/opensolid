module OpenSolid.Binary
  ( ByteString
  , Builder
  , bytes
  , empty
  , concat
  , collect
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.List qualified as List
import Prelude qualified

bytes :: Builder -> ByteString
bytes = Builder.toLazyByteString >> ByteString.toStrict

empty :: Builder
empty = Prelude.mempty

concat :: List Builder -> Builder
concat = Prelude.mconcat

collect :: (a -> Builder) -> List a -> Builder
collect function list = concat (List.map function list)
