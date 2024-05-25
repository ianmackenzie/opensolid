module IO
  ( return
  , fail
  , onError
  , printLine
  )
where

import Basics
import {-# SOURCE #-} Error (Error)

return :: a -> IO a
fail :: Error x => x -> IO a
printLine :: Text -> IO ()
onError :: (Text -> IO a) -> IO a -> IO a
