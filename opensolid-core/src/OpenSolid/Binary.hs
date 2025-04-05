module OpenSolid.Binary
  ( ByteString
  , Builder
  , bytes
  , empty
  , concat
  , collect
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import OpenSolid.Bootstrap
import OpenSolid.Composition
import Prelude (Foldable)
import Prelude qualified

bytes :: Builder -> ByteString
bytes = Builder.toLazyByteString >> ByteString.toStrict

empty :: Builder
empty = Prelude.mempty

concat :: List Builder -> Builder
concat = Prelude.mconcat

collect :: Foldable list => (a -> Builder) -> list a -> Builder
collect = Prelude.foldMap
