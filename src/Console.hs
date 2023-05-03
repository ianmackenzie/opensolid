module Console (print) where

import Data.Text.IO qualified
import Task qualified
import OpenSolid

print :: Text -> Task Text ()
print text = Task.fromIO (Data.Text.IO.putStrLn text) |> Task.mapError errorMessage
