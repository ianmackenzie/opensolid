module File (readFrom, writeTo) where

import OpenSolid
import Task qualified
import Prelude qualified

readFrom :: String -> Task String String
readFrom path = Task.fromIO (Prelude.readFile path) |> Task.mapError errorMessage

writeTo :: String -> String -> Task String ()
writeTo path string = Task.fromIO (Prelude.writeFile path string) |> Task.mapError errorMessage
