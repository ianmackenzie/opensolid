module Json.Serialization
  ( Serialization (..)
  , encode
  , decode
  )
where

import Json (Json)
import Json.Format (Format)
import Json.Format qualified as Format
import OpenSolid

class Serialization a where
  format :: Format a

encode :: Serialization a => a -> Json
encode = Format.encode format

decode :: Serialization a => Json -> Result String a
decode = Format.decode format

instance Serialization Float where format = Format.float

instance Serialization Int where format = Format.int

instance Serialization Bool where format = Format.bool

instance Serialization String where format = Format.string
