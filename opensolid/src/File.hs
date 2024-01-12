module File
  ( readFrom
  , writeTo
  , delete
  )
where

import OpenSolid
import System.Directory
import Task qualified
import Prelude qualified

readFrom :: String -> Task String String
readFrom path = Task.fromIO (Prelude.readFile path)

writeTo :: String -> String -> Task String ()
writeTo path string = Task.fromIO (Prelude.writeFile path string)

delete :: String -> Task String ()
delete path = Task.fromIO (System.Directory.removeFile path)
