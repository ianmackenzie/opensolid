module File
  ( readFrom
  , writeTo
  , delete
  )
where

import OpenSolid
import System.Directory
import Prelude qualified

readFrom :: String -> IO String
readFrom = Prelude.readFile

writeTo :: String -> String -> IO ()
writeTo = Prelude.writeFile

delete :: String -> IO ()
delete = System.Directory.removeFile
