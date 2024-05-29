module Main (main) where

import Http.Server (Request, Response, ResponseReceived)
import Http.Server qualified as Server
import Http.Server.Request qualified as Request
import Http.Server.Response qualified as Response
import OpenSolid

sayHello :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
sayHello request respond = respond $
  case Request.parameterValues "name" request of
    [name] -> Response.ok [] (Response.text ("Hello, " + name + "!"))
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
