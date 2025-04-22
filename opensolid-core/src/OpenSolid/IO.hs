module OpenSolid.IO
  ( fail
  , map
  , run
  , forEach
  , forEachWithIndex
  , collect
  , collectWithIndex
  , sleep
  , (>>)
  , Bind ((>>=))
  , succeed
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
import OpenSolid.Float qualified as Float
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

run :: Foldable list => list (IO ()) -> IO ()
run = Data.Foldable.fold

forEach :: Foldable list => list a -> (a -> IO ()) -> IO ()
forEach list function = Data.Foldable.foldMap function list

forEachWithIndex :: FoldableWithIndex Int list => list a -> (Int -> a -> IO ()) -> IO ()
forEachWithIndex list function = Data.Foldable.WithIndex.ifoldMap function list

collect :: Traversable list => (a -> IO b) -> list a -> IO (list b)
collect = Prelude.mapM

collectWithIndex :: TraversableWithIndex Int list => (Int -> a -> IO b) -> list a -> IO (list b)
collectWithIndex = Data.Traversable.WithIndex.imapM

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

readBinary :: Text -> IO ByteString
readBinary path = Data.ByteString.readFile (Text.unpack path)

writeBinary :: Text -> Builder -> IO ()
writeBinary path builder = Builder.writeFile (Text.unpack path) builder

readUtf8 :: Text -> IO Text
readUtf8 path = readBinary path >>= Text.decodeUtf8

writeUtf8 :: Text -> Text -> IO ()
writeUtf8 path text = writeBinary path (Text.toUtf8 text)

deleteFile :: Text -> IO ()
deleteFile path = System.Directory.removeFile (Text.unpack path)
