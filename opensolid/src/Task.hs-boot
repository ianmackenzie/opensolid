module Task (Task, toIO) where

import Basics

newtype Task a = Task (IO a)

toIO :: Task a -> IO a
