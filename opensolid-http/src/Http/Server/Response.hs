module Http.Server.Response
  ( Response
  , Status
  , Body
  , ok
  , notFound
  , badRequest
  , methodNotAllowed
  , internalServerError
  , text
  , json
  )
where

import Data.ByteString qualified
import Data.CaseInsensitive qualified
import Network.HTTP.Types (Status)
import Network.HTTP.Types qualified
import Network.HTTP.Types.Status qualified as Status
import Network.Wai (Response)
import Network.Wai qualified
import OpenSolid.Json qualified as Json
import OpenSolid.Json.Format qualified as Json.Format
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

custom :: Status -> List (Text, Text) -> ByteString -> Response
custom status headers bytes =
  Network.Wai.responseLBS status (List.map encodeHeader headers) (Data.ByteString.fromStrict bytes)

encodeHeader :: (Text, Text) -> Network.HTTP.Types.Header
encodeHeader (name, value) =
  (Data.CaseInsensitive.mk (Text.encodeUtf8 name), Text.encodeUtf8 value)

ok :: List (Text, Text) -> Body -> Response
ok headers (Body defaultContentType bytes) =
  custom Status.ok200 (withDefaultContentType defaultContentType headers) bytes

withDefaultContentType :: Text -> List (Text, Text) -> List (Text, Text)
withDefaultContentType defaultContentType headers =
  if List.anySatisfy isContentType headers
    then headers
    else contentType defaultContentType : headers

notFound :: Response
notFound = custom Status.notFound404 [] Data.ByteString.empty

error :: Status -> Text -> Response
error status message = custom status [contentType "text/plain"] (Text.encodeUtf8 message)

badRequest :: Text -> Response
badRequest = error Status.badRequest400

methodNotAllowed :: Response
methodNotAllowed = custom Status.methodNotAllowed405 [] Data.ByteString.empty

internalServerError :: Text -> Response
internalServerError = error Status.internalServerError500

isContentType :: (Text, Text) -> Bool
isContentType (name, _) = Text.toLower name == "content-type"

contentType :: Text -> (Text, Text)
contentType = ("Content-Type",)

data Body = Body Text ByteString

text :: Text -> Body
text content = Body "text/plain" (Text.encodeUtf8 content)

json :: Json.Format a -> a -> Body
json format value = Body "application/json" (Json.encode (Json.Format.encode format value))
