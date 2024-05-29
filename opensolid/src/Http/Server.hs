module Http.Server
  ( Application
  , Request
  , Response
  , ResponseReceived
  , runOnPort
  , handle
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified
import IO qualified
import Network.Wai (Application, Request, Response, ResponseReceived)
import Network.Wai qualified
import Network.Wai.Handler.Warp qualified as Warp
import OpenSolid
import Text qualified

handle ::
  Request ->
  (ByteString -> (Response -> IO ResponseReceived) -> IO ResponseReceived) ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
handle request callback respond = IO.do
  bytes <- Network.Wai.consumeRequestBodyStrict request
  callback (Data.ByteString.toStrict bytes) respond

runOnPort :: Int -> Application -> IO ()
runOnPort portNumber application = IO.do
  let settings =
        Warp.defaultSettings
          |> Warp.setPort portNumber
          |> Warp.setBeforeMainLoop (IO.printLine ("Listening on port " + Text.int portNumber))
  Warp.runSettings settings application
