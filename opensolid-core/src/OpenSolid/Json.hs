module OpenSolid.Json
  ( Json (.., OpenSolid.Json.Int, Object)
  , Field (Field)
  , field
  , object
  , text
  , int
  , number
  , bool
  , list
  , listOf
  , map
  , toBinary
  )
where

import Data.Coerce qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Json
  = Null
  | Bool Bool
  | Number Number
  | Text Text
  | List (List Json)
  | Map (Map Text Json)
  deriving (Eq, Show)

newtype Field = KeyValuePair (Text, Json)

pattern Field :: Text -> Json -> Field
pattern Field key value <- KeyValuePair (key, value)

pattern Int :: Int -> Json
pattern Int n <- (toInt -> Just n)

toInt :: Json -> Maybe Int
toInt (Number value) = do
  let rounded = Number.round value
  if Number.fromInt rounded == value then Just rounded else Nothing
toInt _ = Nothing

pattern Object :: List Field -> Json
pattern Object fields <- (getFields -> Just fields)

getFields :: Json -> Maybe (List Field)
getFields (Map fields) = Just (Data.Coerce.coerce (Map.toList fields))
getFields _ = Nothing

{-# COMPLETE Null, Bool, Number, Text, List, Object #-}

int :: Int -> Json
int = Number . Number.fromInt

number :: Number -> Json
number = Number

bool :: Bool -> Json
bool = Bool

text :: Text -> Json
text = Text

list :: List Json -> Json
list = List

listOf :: (a -> Json) -> List a -> Json
listOf encodeItem items = List (List.map encodeItem items)

field :: Text -> Json -> Field
field key value = KeyValuePair (key, value)

object :: List Field -> Json
object fields = Map (Map.fromList (Data.Coerce.coerce fields))

map :: Map Text Json -> Json
map = Map

commaBuilder :: Builder
commaBuilder = Text.toUtf8 ","

quoteBuilder :: Builder
quoteBuilder = Text.toUtf8 "\""

quotedTextBuilder :: Text -> Builder
quotedTextBuilder value = do
  -- Best-effort JSON string escaping
  -- TODO: switch back to Aeson once it's updated to be compatible with GHC 9.14
  let escapedText =
        value
          & Text.replace "\\" "\\\\"
          & Text.replace "\"" "\\\""
          & Text.replace "\n" "\\n"
          & Text.replace "\r" "\\r"
          & Text.replace "\t" "\\t"
          & Text.replace "\b" "\\b"
          & Text.replace "\f" "\\f"
  quoteBuilder <> Text.toUtf8 escapedText <> quoteBuilder

toBinary :: Json -> Builder
toBinary Null = Text.toUtf8 "null"
toBinary (Bool True) = Text.toUtf8 "true"
toBinary (Bool False) = Text.toUtf8 "false"
toBinary (Number value) = Text.toUtf8 (Text.number value)
toBinary (Text value) = quotedTextBuilder value
toBinary (List value) = do
  let itemBuilders = List.map toBinary value
  Text.toUtf8 "[" <> Binary.concat (List.intersperse commaBuilder itemBuilders) <> Text.toUtf8 "]"
toBinary (Map value) = do
  let itemBuilder (name, item) = quotedTextBuilder name <> Text.toUtf8 ":" <> toBinary item
  let itemBuilders = List.map itemBuilder (Map.toList value)
  Text.toUtf8 "{" <> Binary.concat (List.intersperse commaBuilder itemBuilders) <> Text.toUtf8 "}"
