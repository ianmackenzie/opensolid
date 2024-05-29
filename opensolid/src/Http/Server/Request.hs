module Http.Server.Request
  ( Request
  , method
  , path
  , headers
  , headerValues
  , parameters
  , parameterValues
  )
where

import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified
import List qualified
import Maybe qualified
import Network.HTTP.Types qualified
import Network.Wai (Request)
import Network.Wai qualified
import OpenSolid
import Result qualified
import Text qualified

assumeUtf8 :: ByteString -> Text
assumeUtf8 = Text.decodeUtf8 >> Result.handleError Text.acceptReplacementCharacters

method :: Request -> Text
method = Network.Wai.requestMethod >> assumeUtf8 >> Text.toUpper

path :: Request -> List Text
path = Network.Wai.pathInfo

headers :: Request -> List (Text, Text)
headers request =
  Network.Wai.requestHeaders request |> List.map header

header :: Network.HTTP.Types.Header -> (Text, Text)
header (nameCI, valueBytes) =
  (assumeUtf8 (Data.CaseInsensitive.original nameCI), assumeUtf8 valueBytes)

headerValues :: Text -> Request -> List Text
headerValues name request = do
  let nameCI = Data.CaseInsensitive.mk (Text.encodeUtf8 name)
  Network.Wai.requestHeaders request |> Maybe.collect (headerValue nameCI)

headerValue :: Data.CaseInsensitive.CI ByteString -> Network.HTTP.Types.Header -> Maybe Text
headerValue givenName (name, valueBytes) =
  if name == givenName then Just (assumeUtf8 valueBytes) else Nothing

parameters :: Request -> List (Text, Maybe Text)
parameters request =
  Network.Wai.queryString request |> List.map parameter

parameter :: Network.HTTP.Types.QueryItem -> (Text, Maybe Text)
parameter (nameBytes, maybeValueBytes) =
  (assumeUtf8 nameBytes, Maybe.map assumeUtf8 maybeValueBytes)

parameterValues :: Text -> Request -> List Text
parameterValues name request = do
  let nameBytes = Text.encodeUtf8 name
  Network.Wai.queryString request |> Maybe.collect (parameterValue nameBytes)

parameterValue :: ByteString -> Network.HTTP.Types.QueryItem -> Maybe Text
parameterValue givenName (name, valueBytes) =
  if name == givenName then Maybe.map assumeUtf8 valueBytes else Nothing
