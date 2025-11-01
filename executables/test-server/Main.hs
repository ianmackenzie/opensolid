module Main (main) where

import OpenSolid.Http.Request (Request)
import OpenSolid.Http.Request qualified as Request
import OpenSolid.Http.Response (Response)
import OpenSolid.Http.Response qualified as Response
import OpenSolid.Http.Server (ResponseReceived)
import OpenSolid.Http.Server qualified as Server

sayHello :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
sayHello request respond = respond $
  case Request.parameterValues "name" request of
    [name] -> Response.ok [] (Response.text ("Hello, " <> name <> "!"))
    [] -> Response.badRequest "Expected a 'name' query parameter"
    _ -> Response.badRequest "Got multiple 'name' query parameters"

main :: IO ()
main =
  Server.runOnPort 8000 $
    \request respond -> case Request.method request of
      "GET" -> case Request.path request of
        ["hello"] -> sayHello request respond
        _ -> respond Response.notFound
      _ -> respond Response.methodNotAllowed
