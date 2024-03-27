module IO
  ( fail
  , map
  , forEach
  , collect
  , parallel
  , async
  , await
  , sleep
  , (>>)
  , Bind ((>>=))
  , return
  , onError
  , printLine
  )
where

import Basics
import Composition
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import {-# SOURCE #-} Duration (Duration)
import {-# SOURCE #-} Duration qualified
import Error qualified
import Float qualified
import Result (Result (Error, Ok))
import System.IO.Error qualified
import Prelude qualified

fail :: String -> IO a
fail = Prelude.fail

return :: a -> IO a
return = Prelude.return

class Bind m where
  (>>=) :: m a -> (a -> IO b) -> IO b

instance Bind IO where
  (>>=) = (Prelude.>>=)

instance Bind (Result x) where
  Ok value >>= function = function value
  Error error >>= _ = fail (Error.message error)

map :: (a -> b) -> IO a -> IO b
map = Prelude.fmap

forEach :: List a -> (a -> IO ()) -> IO ()
forEach [] _ = return ()
forEach (first : rest) function = function first >> forEach rest function

collect :: (a -> IO b) -> List a -> IO (List b)
collect _ [] = return []
collect function (first : rest) = IO.do
  firstValue <- function first
  restValues <- collect function rest
  return (firstValue : restValues)

parallel :: (a -> IO b) -> List a -> IO (List b)
parallel = Async.mapConcurrently

async :: IO a -> IO (Async a)
async = Async.async

await :: Async a -> IO a
await = Async.wait

sleep :: Duration -> IO ()
sleep duration = Control.Concurrent.threadDelay (Float.round (Duration.inMicroseconds duration))

onError :: (String -> IO a) -> IO a -> IO a
onError callback io =
  System.IO.Error.catchIOError io (System.IO.Error.ioeGetErrorString >> callback)

printLine :: String -> IO ()
printLine = Prelude.putStrLn
