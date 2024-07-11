module IO
  ( succeed
  , fail
  , onError
  , printLine
  )
where

import Basics
import {-# SOURCE #-} Error qualified

succeed :: a -> IO a
fail :: Error.Message x => x -> IO a
printLine :: Text -> IO ()
onError :: (Text -> IO a) -> IO a -> IO a
