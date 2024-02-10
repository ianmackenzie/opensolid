module Task
  ( Task
  , Async
  , evaluate
  , main
  , fail
  , map
  , mapError
  , forEach
  , liftIO
  , fromIO
  , toIO
  , collect
  , parallel
  , spawn
  , await
  , sleep
  , unsafe
  )
where

import Basics hiding (fail)
import Composition (Composition ((>>)))
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Exception qualified
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified
import {-# SOURCE #-} Duration (Duration)
import {-# SOURCE #-} Duration qualified
import Float qualified
import Result (ErrorMessage (errorMessage), Result (Error, Ok))
import Result qualified
import System.Exit qualified
import Prelude (Applicative, Functor, Monad, MonadFail)
import Prelude qualified

newtype Task x a = Task (IO (Result x a))

newtype Async x a = Async (Async.Async (Result x a))

instance Functor (Task x) where
  fmap = map

instance Applicative (Task x) where
  pure value = Task (return (Ok value))
  Task functionIO <*> Task valueIO =
    Task <| do
      functionResult <- functionIO
      valueResult <- valueIO
      return (Result.map2 (<|) functionResult valueResult)

instance Monad (Task x) where
  Task io >>= function =
    Task <| do
      result <- io
      case result of
        Ok value -> toIO (function value)
        Error error -> return (Error error)

instance MonadFail (Task String) where
  fail error = Task (return (Error error))

instance MonadIO (Task IOError) where
  liftIO = liftIO

instance m ~ Task x => Composition (Task x a) (m b) (m b) where
  (>>) = (Prelude.>>)

fail :: ErrorMessage x => x -> Task x a
fail error = Task (return (Error error))

evaluate :: Result x a -> Task x a
evaluate result = Task (return result)

map :: (a -> b) -> Task x a -> Task x b
map function (Task io) = Task (fmap (Result.map function) io)

mapError :: ErrorMessage y => (x -> y) -> Task x a -> Task y a
mapError function (Task io) = Task (fmap (Result.mapError function) io)

unsafe :: IO (Result x a) -> Task x a
unsafe = Task

liftIO :: IO a -> Task IOError a
liftIO io = Task (Control.Exception.catch (fmap Ok io) (Error >> return))

fromIO :: IO a -> Task String a
fromIO = liftIO >> mapError errorMessage

toIO :: Task x a -> IO (Result x a)
toIO (Task io) = io

main :: Task x () -> IO ()
main (Task io) = do
  result <- io
  case result of
    Ok () -> System.Exit.exitSuccess
    Error error -> System.Exit.die (errorMessage error)

forEach :: List a -> (a -> Task x ()) -> Task x ()
forEach values function = Prelude.mapM_ function values

collect :: (a -> Task x b) -> List a -> Task x (List b)
collect = Prelude.mapM

parallel :: (a -> Task x b) -> List a -> Task x (List b)
parallel function values =
  Task (fmap Result.combine (Async.mapConcurrently (function >> toIO) values))

spawn :: Task x a -> Task String (Async x a)
spawn (Task io) = fromIO (fmap Async (Async.async io))

await :: Async x a -> Task x a
await (Async async) = Task (Async.wait async)

sleep :: Duration -> Task String ()
sleep duration =
  let microseconds = Float.round (Duration.inMicroseconds duration)
   in fromIO (Control.Concurrent.threadDelay microseconds)
