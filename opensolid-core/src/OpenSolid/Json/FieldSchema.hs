module OpenSolid.Json.FieldSchema (FieldSchema (FieldSchema, name, required, schema)) where

import OpenSolid.Json qualified as Json
import OpenSolid.Prelude

data FieldSchema = FieldSchema
  { name :: Text
  , required :: Bool
  , schema :: Json.Schema
  }
