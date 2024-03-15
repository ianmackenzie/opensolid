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
import Control.Monad (join)
import OpenSolid hiding (pure, return, (>>))
import Pair qualified
import Task qualified
import Prelude (fmap)

pure :: a -> Task a
pure = Task.pure

return :: a -> Task a
return = Task.return

fail :: String -> Task a
fail = Task.fail

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
