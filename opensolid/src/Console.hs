module Console (printLine) where

import OpenSolid
import Prelude qualified

printLine :: String -> IO ()
printLine = Prelude.putStrLn
