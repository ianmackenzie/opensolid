module IO
  ( return
  , fail
  , onError
  , printLine
  )
where

import Basics

return :: a -> IO a
fail :: String -> IO a
printLine :: String -> IO ()
onError :: (String -> IO a) -> IO a -> IO a
