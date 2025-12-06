module OpenSolid.IO
  ( succeed
  , fail
  , map
  , run
  , maybe
  , forEach
  , forEachWithIndex
  , collect
  , collectWithIndex
  , sleep
  , onError
  , attempt
  , mapError
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
import Data.Traversable.WithIndex (TraversableWithIndex)
import Data.Traversable.WithIndex qualified
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.Prelude
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

forEach :: List a -> (a -> IO ()) -> IO ()
forEach = Data.Foldable.forM_

forEachWithIndex :: List a -> (Int -> a -> IO ()) -> IO ()
forEachWithIndex = Data.Foldable.WithIndex.iforM_

collect :: Traversable list => (a -> IO b) -> list a -> IO (list b)
collect = Prelude.mapM

collectWithIndex :: TraversableWithIndex Int list => (Int -> a -> IO b) -> list a -> IO (list b)
collectWithIndex = Data.Traversable.WithIndex.imapM

sleep :: Duration -> IO ()
sleep duration = Control.Concurrent.threadDelay (round (Duration.inMicroseconds duration))

onError :: (Text -> IO a) -> IO a -> IO a
onError callback io = System.IO.Error.catchIOError io do
  \error -> callback (Text.pack (System.IO.Error.ioeGetErrorString error))

attempt :: IO a -> IO (Result Text a)
attempt io = onError (succeed . Error) (map Ok io)

mapError :: (Text -> Text) -> IO a -> IO a
mapError function = onError (fail . function)

printLine :: Text -> IO ()
printLine = Data.Text.IO.Utf8.putStrLn

readBinary :: Text -> IO ByteString
readBinary path = Data.ByteString.readFile (Text.unpack path)

writeBinary :: Text -> Builder -> IO ()
writeBinary path builder = Builder.writeFile (Text.unpack path) builder

readUtf8 :: Text -> IO Text
readUtf8 path = do
  bytes <- readBinary path
  Result.orFail (Text.decodeUtf8 bytes)

writeUtf8 :: Text -> Text -> IO ()
writeUtf8 path text = do
  let bytes = Text.toUtf8 text
  writeBinary path bytes

deleteFile :: Text -> IO ()
deleteFile path = System.Directory.removeFile (Text.unpack path)

pathSeparator :: Text
pathSeparator = Text.char System.FilePath.pathSeparator
