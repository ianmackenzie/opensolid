module File
  ( readFrom
  , writeTo
  , delete
  )
where

import Data.ByteString qualified
import IO qualified
import OpenSolid
import System.Directory
import Text qualified

readFrom :: Text -> IO Text
readFrom path = IO.do
  bytes <- Data.ByteString.readFile (Text.unpack path)
  Text.decodeUtf8 bytes

writeTo :: Text -> Text -> IO ()
writeTo path contents = Data.ByteString.writeFile (Text.unpack path) (Text.encodeUtf8 contents)

delete :: Text -> IO ()
delete path = System.Directory.removeFile (Text.unpack path)
