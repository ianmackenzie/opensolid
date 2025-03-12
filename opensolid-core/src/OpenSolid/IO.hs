module OpenSolid.IO
  ( fail
  , map
  , forEach
  , forEachWithIndex
  , collect
  , collectWithIndex
  , parallel
  , parallelWithIndex
  , async
  , await
  , sleep
  , (>>)
  , Bind ((>>=))
  , succeed
  , onError
  , attempt
  , mapError
  , addContext
  , printLine
  , time
  , readFile
  , writeFile
  , readBinaryFile
  , writeBinaryFile
  , deleteFile
  )
where

import Control.Concurrent
import Control.Concurrent.Async qualified as Async
import Data.ByteString qualified
import Data.Text.IO.Utf8 qualified
import Data.Time.Clock qualified
import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.Error qualified as Error
import OpenSolid.Float qualified as Float
import OpenSolid.List qualified as List
import OpenSolid.Result (Result (Failure, Success))
import OpenSolid.Result qualified as Result
import OpenSolid.Text qualified as Text
import System.Directory
import System.IO.Error qualified
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

forEachWithIndex :: List a -> (Int -> a -> IO ()) -> IO ()
forEachWithIndex list function =
  forEach (List.indexed list) (\(index, item) -> function index item)

collect :: Traversable list => (a -> IO b) -> list a -> IO (list b)
collect = Prelude.mapM

collectWithIndex :: (Int -> a -> IO b) -> List a -> IO (List b)
collectWithIndex function list =
  collect (\(index, item) -> function index item) (List.indexed list)

parallel :: (a -> IO b) -> List a -> IO (List b)
parallel = Async.mapConcurrently

parallelWithIndex :: (Int -> a -> IO b) -> List a -> IO (List b)
parallelWithIndex function list =
  parallel (\(index, item) -> function index item) (List.indexed list)

async :: IO a -> IO (Async a)
async = Async.async

await :: Async a -> IO a
await = Async.wait

sleep :: Duration -> IO ()
sleep duration = Control.Concurrent.threadDelay (Float.round (Duration.inMicroseconds duration))

onError :: (Text -> IO a) -> IO a -> IO a
onError callback io =
  System.IO.Error.catchIOError io (System.IO.Error.ioeGetErrorString >> Text.pack >> callback)

attempt :: IO a -> IO (Result Text a)
attempt io = map Success io |> onError (succeed . Failure)

mapError :: (Text -> Text) -> IO a -> IO a
mapError function = onError (function >> fail)

addContext :: Text -> IO a -> IO a
addContext text = mapError (Error.addContext text)

printLine :: Text -> IO ()
printLine = Data.Text.IO.Utf8.putStrLn

time :: IO a -> IO (a, Duration)
time io = OpenSolid.IO.do
  startTime <- Data.Time.Clock.getCurrentTime
  result <- io
  endTime <- Data.Time.Clock.getCurrentTime
  succeed (result, Duration.from startTime endTime)

readBinaryFile :: Text -> IO ByteString
readBinaryFile path = Data.ByteString.readFile (Text.unpack path)

writeBinaryFile :: Text -> ByteString -> IO ()
writeBinaryFile path bytes = Data.ByteString.writeFile (Text.unpack path) bytes

readFile :: Text -> IO Text
readFile path = readBinaryFile path >>= Text.decodeUtf8

writeFile :: Text -> Text -> IO ()
writeFile path contents = writeBinaryFile path (Text.encodeUtf8 contents)

deleteFile :: Text -> IO ()
deleteFile path = System.Directory.removeFile (Text.unpack path)
