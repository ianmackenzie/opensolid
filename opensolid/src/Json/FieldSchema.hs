module Json.FieldSchema (FieldSchema (..)) where

import Json.Schema (Schema)
import OpenSolid

data FieldSchema = FieldSchema
  { name :: String
  , required :: Bool
  , schema :: Schema
  }
