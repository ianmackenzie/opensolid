module IO
  ( fail
  , map
  , forEach
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
  , printLine
  )
where

import Basics hiding (pure, return)
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Control.Monad (join)
import {-# SOURCE #-} Duration (Duration)
import {-# SOURCE #-} Duration qualified
import Error (Error)
import Error qualified
import Float qualified
import Result (Result (Error, Ok))
import System.IO.Error qualified
import Prelude (fail, fmap, pure, return)
import Prelude qualified

class Bind m where
  (>>=) :: m a -> (a -> IO b) -> IO b

instance Bind IO where
  (>>=) = (Prelude.>>=)

instance Bind (Result x) where
  Ok value >>= function = function value
  Error error >>= _ = fail (Error.message error)

class Apply m n where
  (<*>) :: m (a -> b) -> n a -> IO b

instance Apply IO IO where
  (<*>) = (Prelude.<*>)

instance Error x => Apply (Result x) IO where
  result <*> io = evaluate result <*> io

instance Error x => Apply IO (Result x) where
  io <*> result = io <*> evaluate result

instance (Error x, Error y) => Apply (Result x) (Result y) where
  result1 <*> result2 = evaluate result1 <*> evaluate result2

class Sequence m where
  (>>) :: m () -> IO a -> IO a

instance Sequence IO where
  (>>) = (Prelude.>>)

instance Sequence (Result x) where
  Ok () >> io = io
  Error error >> _ = fail (Error.message error)

evaluate :: Error x => Result x a -> IO a
evaluate (Ok value) = return value
evaluate (Error error) = fail (Error.message error)

map :: (a -> b) -> IO a -> IO b
map = Prelude.fmap

forEach :: List a -> (a -> IO ()) -> IO ()
forEach [] _ = return ()
forEach (first : rest) function = IO.do
  function first
  forEach rest function

collect :: (a -> IO b) -> List a -> IO (List b)
collect _ [] = return []
collect function (first : rest) = IO.do
  firstValue <- function first
  restValues <- collect function rest
  return (firstValue : restValues)

parallel :: (a -> IO b) -> List a -> IO (List b)
parallel = Async.mapConcurrently

spawn :: IO a -> IO (Async a)
spawn = Async.async

await :: Async a -> IO a
await = Async.wait

sleep :: Duration -> IO ()
sleep duration = Control.Concurrent.threadDelay (Float.round (Duration.inMicroseconds duration))

onError :: (String -> IO a) -> IO a -> IO a
onError callback io =
  System.IO.Error.catchIOError io (\ioError -> callback (System.IO.Error.ioeGetErrorString ioError))

printLine :: String -> IO ()
printLine = Prelude.putStrLn
