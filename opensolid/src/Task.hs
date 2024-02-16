module Task
  ( Task
  , Async
  , evaluate
  , check
  , fail
  , map
  , forEach
  , fromIO
  , toIO
  , collect
  , parallel
  , spawn
  , await
  , sleep
  , Bind ((>>=))
  , Apply ((<*>))
  , Sequence ((>>))
  , fmap
  , join
  , pure
  , return
  , onError
  , debug
  )
where

import Basics
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified
import {-# SOURCE #-} Duration (Duration)
import {-# SOURCE #-} Duration qualified
import Error (Error)
import Error qualified
import Float qualified
import Result (Result (Error, Ok))
import System.IO.Error qualified
import Prelude (Applicative, Functor, Monad, MonadFail, fmap)
import Prelude qualified

newtype Task a = Task (IO a)

newtype Async a = Async (Async.Async a)

instance Functor Task where
  fmap = map

instance Applicative Task where
  pure = pure
  (<*>) = (<*>)

pure :: a -> Task a
pure value = Task (return value)

instance Monad Task where
  (>>=) = (>>=)

instance MonadFail Task where
  fail = fail

fail :: String -> Task a
fail message = Task (Prelude.fail message)

instance MonadIO Task where
  liftIO = fromIO

instance Error.Map String String Task Task where
  map function = onError (\error -> fail (function error))

class Bind m where
  (>>=) :: m a -> (a -> Task b) -> Task b

instance Bind Task where
  Task io >>= function =
    Task (io Prelude.>>= (\value -> toIO (function value)))

instance Bind (Result x) where
  Ok value >>= function = function value
  Error error >>= _ = fail (Error.message error)

class Apply m n where
  (<*>) :: m (a -> b) -> n a -> Task b

instance Apply Task Task where
  Task functionIO <*> Task valueIO = Task (functionIO Prelude.<*> valueIO)

instance Error x => Apply (Result x) Task where
  result <*> task = evaluate result <*> task

instance Error x => Apply Task (Result x) where
  task <*> result = task <*> evaluate result

instance (Error x, Error y) => Apply (Result x) (Result y) where
  result1 <*> result2 = evaluate result1 <*> evaluate result2

class Sequence m where
  (>>) :: m () -> Task b -> Task b

instance Sequence Task where
  (>>) = (Prelude.>>)

instance Sequence (Result x) where
  Ok () >> task = task
  Error error >> _ = fail (Error.message error)

evaluate :: Error x => Result x a -> Task a
evaluate (Ok value) = Task (return value)
evaluate (Error error) = Task (Prelude.fail (Error.message error))

check :: Bool -> String -> Task ()
check condition message = if condition then return () else fail message

map :: (a -> b) -> Task a -> Task b
map function (Task io) = Task (Prelude.fmap function io)

fromIO :: IO a -> Task a
fromIO = Task

toIO :: Task a -> IO a
toIO (Task io) = io

forEach :: List a -> (a -> Task ()) -> Task ()
forEach [] _ = return ()
forEach (first : rest) function = Task.do
  function first
  forEach rest function

collect :: (a -> Task b) -> List a -> Task (List b)
collect _ [] = return []
collect function (first : rest) = Task.do
  firstValue <- function first
  restValues <- collect function rest
  return (firstValue : restValues)

parallel :: (a -> Task b) -> List a -> Task (List b)
parallel function values =
  Task (Async.mapConcurrently (\value -> toIO (function value)) values)

spawn :: Task a -> Task (Async a)
spawn (Task io) = Task (Prelude.fmap Async (Async.async io))

await :: Async a -> Task a
await (Async async) = Task (Async.wait async)

sleep :: Duration -> Task ()
sleep duration =
  let microseconds = Float.round (Duration.inMicroseconds duration)
   in Task (Control.Concurrent.threadDelay microseconds)

onError :: (String -> Task a) -> Task a -> Task a
onError callback (Task io) =
  Task <|
    System.IO.Error.catchIOError io <|
      \ioError -> toIO (callback (System.IO.Error.ioeGetErrorString ioError))

class Debug m where
  debug :: Task () -> m a -> Task a

instance Debug Task where
  debug debugTask = onError (\error -> Task.do debugTask; fail error)

instance Error x => Debug (Result x) where
  debug debugTask result = debug debugTask (evaluate result)
