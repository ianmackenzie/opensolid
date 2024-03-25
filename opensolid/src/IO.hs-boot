module IO (onError) where

import Basics

onError :: (String -> IO a) -> IO a -> IO a
