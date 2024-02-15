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
import OpenSolid hiding ((>>))
import Pair qualified
import Task qualified
import Prelude qualified

pure :: a -> Task a
pure = Task.pure

fmap :: (a -> b) -> Task a -> Task b
fmap = Task.fmap

(>>=) :: Task.Bind m => m a -> (a -> Task b) -> Task b
(>>=) = (Task.>>=)

(>>) :: Task a -> Task b -> Task b
task1 >> task2 =
  Task.fromIO <|
    Prelude.fmap Pair.second <|
      Async.concurrently (Task.toIO task1) (Task.toIO task2)

(<*>) :: Task (a -> b) -> Task a -> Task b
functionTask <*> valueTask =
  Task.fromIO <|
    Prelude.fmap (\(function, value) -> function value) <|
      Async.concurrently (Task.toIO functionTask) (Task.toIO valueTask)
