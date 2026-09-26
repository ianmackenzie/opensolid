module OpenSolid.IO
  ( succeed
  , fail
  , map
  , run
  , maybe
  , collect
  , forEach
  , forEachWithIndex
  , sleep
  , onError
  , attempt
  , mapError
  , bracket
  , printLine
  , readUtf8
  , writeUtf8
  , readBinary
  , writeBinary
  , deleteFile
  , pathSeparator
  )
where

import Control.Concurrent qualified
import Control.Exception qualified
import Data.ByteString qualified
import Data.ByteString.Builder qualified as Builder
import Data.Foldable qualified
import Data.Text.IO.Utf8 qualified
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding (forEach, forEachWithIndex)
import OpenSolid.Prelude qualified
import OpenSolid.Result qualified as Result
import OpenSolid.Text qualified as Text
import System.Directory
import System.FilePath qualified
import System.IO.Error qualified
import Prelude qualified

succeed :: a -> IO a
succeed = Prelude.return

fail :: Text -> IO a
fail message = Prelude.fail (Text.unpack message)

map :: (a -> b) -> IO a -> IO b
map = Prelude.fmap

run :: List (IO ()) -> IO ()
run = Data.Foldable.fold

maybe :: (a -> IO ()) -> Maybe a -> IO ()
maybe _ Nothing = succeed ()
maybe callback (Just value) = callback value

collect :: Traversable list => (a -> IO b) -> list a -> IO (list b)
collect = Prelude.mapM

forEach :: Foldable list => list a -> (a -> IO ()) -> IO ()
forEach list function =
  succeed () & OpenSolid.Prelude.forEach list \item -> (>> function item)

forEachWithIndex :: Foldable list => list a -> (Int -> a -> IO ()) -> IO ()
forEachWithIndex list function =
  succeed () & OpenSolid.Prelude.forEachWithIndex list \index item -> (>> function index item)

sleep :: Duration -> IO ()
sleep duration = Control.Concurrent.threadDelay (Number.round (Duration.inMicroseconds duration))

onError :: (Text -> IO a) -> IO a -> IO a
onError callback io =
  System.IO.Error.catchIOError io do
    callback . Text.pack . System.IO.Error.ioeGetErrorString

attempt :: IO a -> IO (Result Text a)
attempt io = onError (succeed . Err) (map Ok io)

mapError :: (Text -> Text) -> IO a -> IO a
mapError function = onError (function >> fail)

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket = Control.Exception.bracket

printLine :: Text -> IO ()
printLine = Data.Text.IO.Utf8.putStrLn

readBinary :: Text -> IO ByteString
readBinary path = Data.ByteString.readFile (Text.unpack path)

writeBinary :: Text -> Builder -> IO ()
writeBinary path builder = Builder.writeFile (Text.unpack path) builder

readUtf8 :: Text -> IO Text
readUtf8 path = do
  bytes <- readBinary path
  Text.decodeUtf8 bytes & Result.orFail

writeUtf8 :: Text -> Text -> IO ()
writeUtf8 path text = do
  let bytes = Text.toUtf8 text
  writeBinary path bytes

deleteFile :: Text -> IO ()
deleteFile path = System.Directory.removeFile (Text.unpack path)

pathSeparator :: Text
pathSeparator = Text.char System.FilePath.pathSeparator
