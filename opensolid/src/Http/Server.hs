module Http.Server (Application, runOnPort, handle) where

import Data.ByteString (ByteString)
import Data.ByteString qualified
import IO qualified
import Network.Wai (Application, Request, Response, ResponseReceived)
import Network.Wai qualified
import Network.Wai.Handler.Warp qualified as Warp
import OpenSolid

handle ::
  Request ->
  (ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived) ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
handle request callback respond = IO.do
  bytes <- Network.Wai.consumeRequestBodyStrict request
  callback (Data.ByteString.toStrict bytes) respond

runOnPort :: Int -> Application -> IO ()
runOnPort = Warp.run
