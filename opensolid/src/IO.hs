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
  , mapError
  , debugError
  , addContext
  , printLine
  )
where

import Basics
import Composition
import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Data.ByteString.Char8 qualified
import Debug qualified
import Duration (Duration)
import Duration qualified
import Error qualified
import Float qualified
import Result (Result (Failure, Success))
import System.IO.Error qualified
import Text qualified
import Prelude qualified

fail :: Error.Message x => x -> IO a
fail error = Prelude.fail (Text.unpack (Error.message error))

return :: a -> IO a
return = Prelude.return

class Bind m where
  (>>=) :: m a -> (a -> IO b) -> IO b

instance Bind IO where
  (>>=) = (Prelude.>>=)

instance Bind (Result x) where
  Success value >>= function = function value
  Failure error >>= _ = fail error

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

onError :: (Text -> IO a) -> IO a -> IO a
onError callback io =
  System.IO.Error.catchIOError io (System.IO.Error.ioeGetErrorString >> Text.pack >> callback)

debugError :: (Text -> IO ()) -> IO a -> IO a
debugError callback = onError (\error -> Debug.io (callback error) >> fail error)

mapError :: (Text -> Text) -> IO a -> IO a
mapError function = onError (function >> fail)

addContext :: Text -> IO a -> IO a
addContext text = mapError (Error.addContext text)

printLine :: Text -> IO ()
printLine = Text.encodeUtf8 >> Data.ByteString.Char8.putStrLn
