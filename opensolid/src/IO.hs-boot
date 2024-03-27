module IO (fail, onError, printLine) where

import Basics

fail :: String -> IO a
printLine :: String -> IO ()
onError :: (String -> IO a) -> IO a -> IO a
