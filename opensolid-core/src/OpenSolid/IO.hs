module OpenSolid.IO
  ( try
  , map
  , run
  , forEach
  , forEachWithIndex
  , collect
  , collectWithIndex
  , sleep
  , onError
  , attempt
  , mapError
  , addContext
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
import Data.ByteString qualified
import Data.ByteString.Builder qualified as Builder
import Data.Foldable qualified
import Data.Foldable.WithIndex qualified
import Data.Text.IO.Utf8 qualified
import Data.Traversable.WithIndex qualified
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Bootstrap
import OpenSolid.Composition
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.Error qualified as Error
import OpenSolid.Number qualified as Number
import OpenSolid.Result (Result (Failure, Success))
import OpenSolid.Text qualified as Text
import System.Directory
import System.FilePath qualified
import System.IO.Error qualified
import Prelude qualified

try :: Result x a -> IO a
try (Success value) = return value
try (Failure error) = fail (Text.unpack (Error.message error))

map :: (a -> b) -> IO a -> IO b
map = fmap

run :: Foldable list => list (IO ()) -> IO ()
run = Data.Foldable.fold

forEach :: Foldable list => list a -> (a -> IO ()) -> IO ()
forEach = Data.Foldable.forM_

forEachWithIndex :: FoldableWithIndex Int list => list a -> (Int -> a -> IO ()) -> IO ()
forEachWithIndex = Data.Foldable.WithIndex.iforM_

collect :: Traversable list => (a -> IO b) -> list a -> IO (list b)
collect = Prelude.mapM

collectWithIndex :: TraversableWithIndex Int list => (Int -> a -> IO b) -> list a -> IO (list b)
collectWithIndex = Data.Traversable.WithIndex.imapM

sleep :: Duration -> IO ()
sleep duration = Control.Concurrent.threadDelay (Number.round (Duration.inMicroseconds duration))

onError :: (Text -> IO a) -> IO a -> IO a
onError callback io = System.IO.Error.catchIOError io do
  \error -> callback (Text.pack (System.IO.Error.ioeGetErrorString error))

attempt :: IO a -> IO (Result Text a)
attempt io = map Success io |> onError (return . Failure)

mapError :: (Text -> Text) -> IO a -> IO a
mapError function = onError (fail . Text.unpack . function)

addContext :: Text -> IO a -> IO a
addContext text = mapError (Error.addContext text)

printLine :: Text -> IO ()
printLine = Data.Text.IO.Utf8.putStrLn

readBinary :: Text -> IO ByteString
readBinary path = Data.ByteString.readFile (Text.unpack path)

writeBinary :: Text -> Builder -> IO ()
writeBinary path builder = Builder.writeFile (Text.unpack path) builder

readUtf8 :: Text -> IO Text
readUtf8 path = do
  bytes <- readBinary path
  try (Text.decodeUtf8 bytes)

writeUtf8 :: Text -> Text -> IO ()
writeUtf8 path text = do
  let bytes = Text.toUtf8 text
  writeBinary path bytes

deleteFile :: Text -> IO ()
deleteFile path = System.Directory.removeFile (Text.unpack path)

pathSeparator :: Text
pathSeparator = Text.char System.FilePath.pathSeparator
