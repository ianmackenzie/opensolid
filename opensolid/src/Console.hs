module Console (printLine) where

import OpenSolid
import Task qualified
import Prelude qualified

printLine :: String -> Task String ()
printLine string = Task.fromIO (Prelude.putStrLn string) |> Task.mapError errorMessage
