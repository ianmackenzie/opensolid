module Task
  ( Task
  , Async
  , evaluate
  , check
  , fail
  , map
  , mapError
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
import Prelude (Applicative, Functor, Monad, MonadFail)
import Prelude qualified

newtype Task a = Task (IO a)

newtype Async a = Async (Async.Async a)

instance Functor Task where
  fmap = fmap

fmap :: (a -> b) -> Task a -> Task b
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

instance Error.Map String String (Task a) (Task a) where
  map = mapError

class Bind m where
  (>>=) :: m a -> (a -> Task b) -> Task b

instance Bind Task where
  Task io >>= function =
    Task (io Prelude.>>= (\value -> toIO (function value)))

instance Bind (Result x) where
  Ok value >>= function = function value
  Error error >>= _ = fail (Error.message error)

class Apply m where
  (<*>) :: m (a -> b) -> Task a -> Task b

instance Apply Task where
  Task functionIO <*> Task valueIO = Task (functionIO Prelude.<*> valueIO)

instance Apply (Result x) where
  Ok function <*> task = map function task
  Error error <*> _ = fail (Error.message error)

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

mapError :: (String -> String) -> Task a -> Task a
mapError function (Task io) =
  Task <|
    System.IO.Error.catchIOError io <|
      \ioError -> Prelude.fail (function (System.IO.Error.ioeGetErrorString ioError))

fromIO :: IO a -> Task a
fromIO = Task

toIO :: Task a -> IO a
toIO (Task io) = io

forEach :: List a -> (a -> Task ()) -> Task ()
forEach values function = Prelude.mapM_ function values

collect :: (a -> Task b) -> List a -> Task (List b)
collect = Prelude.mapM

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
