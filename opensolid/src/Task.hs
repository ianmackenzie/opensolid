module Task
  ( Task
  , Async
  , evaluate
  , check
  , main
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
  , unsafe
  , (>>=)
  , (<*>)
  , (>>)
  , fmap
  , join
  , pure
  , return
  )
where

import Basics hiding (fail, (<*>), (>>=))
import Composition (Composition)
import Composition qualified
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Exception qualified
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified
import {-# SOURCE #-} Duration (Duration)
import {-# SOURCE #-} Duration qualified
import Error (Error)
import Error qualified
import Float qualified
import Result (Result (Error, Ok))
import Result qualified
import System.Exit qualified
import System.IO.Error qualified
import Prelude (Applicative, Functor, Monad, MonadFail)
import Prelude qualified

newtype Task a = Task (IO (Result String a))

newtype Async a = Async (Async.Async (Result String a))

instance Functor Task where
  fmap = map

instance Applicative Task where
  pure value = Task (return (Ok value))
  Task functionIO <*> Task valueIO =
    Task Prelude.do
      functionResult <- functionIO
      valueResult <- valueIO
      return (Result.map2 (<|) functionResult valueResult)

instance Monad Task where
  Task io >>= function =
    Task Prelude.do
      result <- io
      case result of
        Ok value -> toIO (function value)
        Error error -> return (Error error)

instance MonadFail Task where
  fail = fail

instance MonadIO Task where
  liftIO = fromIO

instance Composition (Task a) (Task b) (Task b) where
  (>>) = (Prelude.>>)

instance Error.Map String String (Task a) (Task a) where
  map = mapError

class Bind a b c | a b -> c where
  (>>=) :: a -> b -> c

instance (a ~ a', taskB ~ Task b) => Bind (Task a) (a' -> taskB) (Task b) where
  (>>=) = (Prelude.>>=)

instance (a ~ a', taskB ~ Task b) => Bind (Result x a) (a' -> taskB) (Task b) where
  Ok value >>= function = function value
  Error error >>= _ = fail (Error.message error)

class Apply a b c | a b -> c where
  (<*>) :: a -> b -> c

instance a ~ a' => Apply (Task (a -> b)) (Task a') (Task b) where
  (<*>) = (Prelude.<*>)

instance a ~ a' => Apply (Result x (a -> b)) (Task a') (Task b) where
  Ok function <*> task = fmap function task
  Error error <*> _ = fail (Error.message error)

class Sequence a b c | a b -> c where
  (>>) :: a -> b -> c

instance Sequence (Task a) (Task b) (Task b) where
  (>>) = (Prelude.>>)

instance Sequence (Result x a) (Task b) (Task b) where
  Ok _ >> task = task
  Error error >> _ = fail (Error.message error)

fail :: String -> Task a
fail message = Task (return (Error message))

evaluate :: Error x => Result x a -> Task a
evaluate result = Task (return (Result.mapError Error.message result))

check :: Bool -> String -> Task ()
check condition message = if condition then return () else fail message

map :: (a -> b) -> Task a -> Task b
map function (Task io) = Task (fmap (Result.map function) io)

mapError :: (String -> String) -> Task a -> Task a
mapError function (Task io) = Task (fmap (Result.mapError function) io)

unsafe :: IO (Result String a) -> Task a
unsafe = Task

fromIO :: IO a -> Task a
fromIO io =
  Task <|
    Control.Exception.catch (fmap Ok io) <|
      (\ioError -> return (Error (System.IO.Error.ioeGetErrorString ioError)))

toIO :: Task a -> IO (Result String a)
toIO (Task io) = io

main :: Task () -> IO ()
main (Task io) = Prelude.do
  result <- io
  case result of
    Ok () -> System.Exit.exitSuccess
    Error message -> System.Exit.die message

forEach :: List a -> (a -> Task ()) -> Task ()
forEach values function = Prelude.mapM_ function values

collect :: (a -> Task b) -> List a -> Task (List b)
collect = Prelude.mapM

parallel :: (a -> Task b) -> List a -> Task (List b)
parallel function values =
  Task (fmap Result.combine (Async.mapConcurrently (\value -> toIO (function value)) values))

spawn :: Task a -> Task (Async a)
spawn (Task io) = fromIO (fmap Async (Async.async io))

await :: Async a -> Task a
await (Async async) = Task (Async.wait async)

sleep :: Duration -> Task ()
sleep duration =
  let microseconds = Float.round (Duration.inMicroseconds duration)
   in fromIO (Control.Concurrent.threadDelay microseconds)
