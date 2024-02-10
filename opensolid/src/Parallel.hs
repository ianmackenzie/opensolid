module Parallel
  ( (<*>)
  , (>>=)
  , (>>)
  , fail
  , return
  , pure
  , join
  , fmap
  )
where

import Control.Concurrent.Async qualified as Async
import OpenSolid hiding ((<*>), (>>))
import OpenSolid qualified
import Pair qualified
import Task qualified

(>>) :: Task x a -> Task x b -> Task x b
task1 >> task2 =
  Task.unsafe <|
    fmap Pair.second <|
      Async.concurrently (Task.toIO task1) (Task.toIO task2)

apply :: (Result x (a -> b), Result x a) -> Result x b
apply (functionResult, valueResult) =
  functionResult OpenSolid.<*> valueResult

(<*>) :: Task x (a -> b) -> Task x a -> Task x b
functionTask <*> valueTask =
  Task.unsafe <|
    fmap apply <|
      Async.concurrently (Task.toIO functionTask) (Task.toIO valueTask)
