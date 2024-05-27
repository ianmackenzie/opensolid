module Json.FieldSchema (FieldSchema (..)) where

import Json qualified
import OpenSolid

data FieldSchema = FieldSchema
  { name :: Text
  , required :: Bool
  , schema :: Json.Schema
  }
