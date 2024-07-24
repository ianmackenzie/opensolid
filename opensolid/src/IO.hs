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
  , succeed
  , onError
  , mapError
  , addContext
  , printLine
  , time
  )
where

import Basics
import Composition
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Data.ByteString.Char8 qualified
import Data.Time.Clock qualified
import Duration (Duration)
import Duration qualified
import Error qualified
import Float qualified
import Result (Result (Failure, Success))
import Result qualified
import System.IO.Error qualified
import Text qualified
import Prelude qualified

fail :: Error.Message x => x -> IO a
fail error = Prelude.fail (Text.unpack (Error.message error))

succeed :: a -> IO a
succeed = Prelude.return

class Bind m1 m2 where
  (>>=) :: m1 a -> (a -> m2 b) -> IO b

instance Bind IO (Result x) where
  io >>= function = io >>= (function >> Result.toIO)

instance Bind IO IO where
  (>>=) = (Prelude.>>=)

instance Bind (Result x) IO where
  Success value >>= function = function value
  Failure error >>= _ = fail error

map :: (a -> b) -> IO a -> IO b
map = Prelude.fmap

forEach :: List a -> (a -> IO ()) -> IO ()
forEach [] _ = succeed ()
forEach (first : rest) function = function first >> forEach rest function

collect :: (a -> IO b) -> List a -> IO (List b)
collect _ [] = succeed []
collect function (first : rest) = IO.do
  firstValue <- function first
  restValues <- collect function rest
  succeed (firstValue : restValues)

parallel :: (a -> IO b) -> List a -> IO (List b)
parallel = Async.mapConcurrently

async :: IO a -> IO (Async a)
async = Async.async

await :: Async a -> IO a
await = Async.wait

sleep :: Duration -> IO ()
sleep duration = Control.Concurrent.threadDelay (Float.round (Duration.inMicroseconds duration))

onError :: (Text -> IO a) -> IO a -> IO a
onError callback io =
  System.IO.Error.catchIOError io (System.IO.Error.ioeGetErrorString >> Text.pack >> callback)

mapError :: (Text -> Text) -> IO a -> IO a
mapError function = onError (function >> fail)

addContext :: Text -> IO a -> IO a
addContext text = mapError (Error.addContext text)

printLine :: Text -> IO ()
printLine = Text.encodeUtf8 >> Data.ByteString.Char8.putStrLn

time :: IO a -> IO (a, Duration)
time io = IO.do
  startTime <- Data.Time.Clock.getCurrentTime
  result <- io
  endTime <- Data.Time.Clock.getCurrentTime
  succeed (result, Duration.from startTime endTime)
