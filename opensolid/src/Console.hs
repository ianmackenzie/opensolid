module Console (printLine) where

import Data.Text.IO qualified
import OpenSolid
import Task qualified

printLine :: Text -> Task Text ()
printLine text = Task.fromIO (Data.Text.IO.putStrLn text) |> Task.mapError errorMessage
