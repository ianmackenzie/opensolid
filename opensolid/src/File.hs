module File
  ( readFrom
  , writeTo
  , delete
  )
where

import Data.Text.IO qualified
import OpenSolid
import System.Directory
import Text qualified

readFrom :: Text -> IO Text
readFrom path = Data.Text.IO.readFile (Text.unpack path)

writeTo :: Text -> Text -> IO ()
writeTo path contents = Data.Text.IO.writeFile (Text.unpack path) contents

delete :: Text -> IO ()
delete path = System.Directory.removeFile (Text.unpack path)
