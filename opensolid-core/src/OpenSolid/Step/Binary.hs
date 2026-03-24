module OpenSolid.Step.Binary (builder) where

import Data.ByteString qualified
import Data.ByteString.Builder qualified
import Data.Char qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.List qualified as List
import OpenSolid.Prelude

{-| Take a binary builder for some arbitrary binary data,
and encode it using method specified in the STEP standard (a form of Base64 encoding).
The result is a new binary builder suitable for direct inclusion in an encoded STEP file.

Note that the leading and trailing double quotation marks
(used when writing binary data to a STEP file)
are *not* included in the result.
-}
builder :: Builder -> Builder
builder givenBuilder = do
  let bytes = List.map fromIntegral (Data.ByteString.unpack (Binary.bytes givenBuilder))
  Data.ByteString.Builder.charUtf8 '0' <> Binary.combine byteBuilder bytes

byteBuilder :: Int -> Builder
byteBuilder byte = nibbleBuilder (byte // 16) <> nibbleBuilder (byte % 16)

nibbleBuilder :: Int -> Builder
nibbleBuilder = Data.ByteString.Builder.charUtf8 . Data.Char.intToDigit
