module OpenSolid.Http.Request
  ( Request
  , method
  , path
  , headers
  , headerValues
  , parameters
  , parameterValues
  )
where

import Data.CaseInsensitive qualified
import Network.HTTP.Types qualified
import Network.Wai (Request)
import Network.Wai qualified
import OpenSolid.Binary (ByteString)
import OpenSolid.Binary qualified as Binary
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

method :: Request -> Text
method = Network.Wai.requestMethod >> Text.assumeUtf8 >> Text.toUpper

path :: Request -> List Text
path = Network.Wai.pathInfo

headers :: Request -> List (Text, Text)
headers request =
  Network.Wai.requestHeaders request |> List.map header

header :: Network.HTTP.Types.Header -> (Text, Text)
header (nameCI, valueBytes) =
  (Text.assumeUtf8 (Data.CaseInsensitive.original nameCI), Text.assumeUtf8 valueBytes)

headerValues :: Text -> Request -> List Text
headerValues name request = do
  let nameCI = Data.CaseInsensitive.mk (Binary.bytes (Text.toUtf8 name))
  Network.Wai.requestHeaders request |> Maybe.collect (headerValue nameCI)

headerValue :: Data.CaseInsensitive.CI ByteString -> Network.HTTP.Types.Header -> Maybe Text
headerValue givenName (name, valueBytes) =
  if name == givenName then Just (Text.assumeUtf8 valueBytes) else Nothing

parameters :: Request -> List (Text, Maybe Text)
parameters request =
  Network.Wai.queryString request |> List.map parameter

parameter :: Network.HTTP.Types.QueryItem -> (Text, Maybe Text)
parameter (nameBytes, maybeValueBytes) =
  (Text.assumeUtf8 nameBytes, Maybe.map Text.assumeUtf8 maybeValueBytes)

parameterValues :: Text -> Request -> List Text
parameterValues name request = do
  let nameBytes = Binary.bytes (Text.toUtf8 name)
  Network.Wai.queryString request |> Maybe.collect (parameterValue nameBytes)

parameterValue :: ByteString -> Network.HTTP.Types.QueryItem -> Maybe Text
parameterValue givenName (name, valueBytes) =
  if name == givenName then Maybe.map Text.assumeUtf8 valueBytes else Nothing
