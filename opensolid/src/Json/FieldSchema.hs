module Json.FieldSchema (FieldSchema (..)) where

import Json.Schema (Schema)
import OpenSolid

data FieldSchema = FieldSchema
  { name :: Text
  , required :: Bool
  , schema :: Schema
  }
