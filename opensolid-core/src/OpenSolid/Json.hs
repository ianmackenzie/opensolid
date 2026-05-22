module OpenSolid.Json
  ( Json
  , Field
  , field
  , object
  , text
  , int
  , number
  , bool
  , list
  , listOf
  , map
  , builder
  )
where

import Data.Coerce qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Json
  = Null
  | Bool Bool
  | Int Int
  | Number Number
  | Text Text
  | List (List Json)
  | Map (Map Text Json)
  deriving (Eq, Show)

newtype Field = Field (Text, Json)

int :: Int -> Json
int = Int

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
field key value = Field (key, value)

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

builder :: Json -> Builder
builder Null = Text.toUtf8 "null"
builder (Bool True) = Text.toUtf8 "true"
builder (Bool False) = Text.toUtf8 "false"
builder (Int value) = Text.toUtf8 (Text.int value)
builder (Number value) = Text.toUtf8 (Text.number value)
builder (Text value) = quotedTextBuilder value
builder (List value) = do
  let itemBuilders = List.map builder value
  Text.toUtf8 "[" <> Binary.concat (List.intersperse commaBuilder itemBuilders) <> Text.toUtf8 "]"
builder (Map value) = do
  let itemBuilder (name, item) = quotedTextBuilder name <> Text.toUtf8 ":" <> builder item
  let itemBuilders = List.map itemBuilder (Map.toList value)
  Text.toUtf8 "{" <> Binary.concat (List.intersperse commaBuilder itemBuilders) <> Text.toUtf8 "}"
