module Main (main) where

import Http.Server qualified as Server
import Http.Server.Request qualified as Request
import Http.Server.Response qualified as Response
import OpenSolid

main :: IO ()
main =
  Server.runOnPort 8000 $
    \request respond ->
      case Request.method request of
        method -> respond (Response.badRequest ("Unsupported method " + method))
