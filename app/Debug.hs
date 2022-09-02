module Debug (log, toString) where

import qualified Data.Text
import qualified Data.Text.IO
import OpenSolid
import qualified String

log :: Show a => String -> a -> IO ()
log label value = Data.Text.IO.putStrLn line
  where
    line = String.concat [label, ": ", toString value]

toString :: Show a => a -> String
toString value = Data.Text.pack (show value)
