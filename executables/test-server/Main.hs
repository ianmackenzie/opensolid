module Main (main) where

import OpenSolid.Http.Request (Request)
import OpenSolid.Http.Request qualified as Request
import OpenSolid.Http.Response (Response)
import OpenSolid.Http.Response qualified as Response
import OpenSolid.Http.Server qualified as Server
import OpenSolid.List qualified as List
import OpenSolid.Prelude

sayHello :: Request -> Response
sayHello request =
  case Request.parameterValues "name" request of
    [] -> Response.badRequest "Expected a 'name' query parameter"
    List.One name -> Response.ok [] (Response.text ("Hello, " <> name <> "!"))
    List.TwoOrMore -> Response.badRequest "Got multiple 'name' query parameters"

main :: IO ()
main =
  Server.runOnPort 8000 \request respond ->
    case Request.method request of
      "GET" -> case Request.path request of
        ["hello"] -> respond (sayHello request)
        _ -> respond Response.notFound
      _ -> respond Response.methodNotAllowed
