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
import Qty qualified
import Units qualified

class Serialization a where
  format :: Format a

encode :: Serialization a => a -> Json
encode = Format.encode format

decode :: Serialization a => Json -> Result String a
decode = Format.decode format

instance Serialization Int where format = Format.int

instance Serialization Bool where format = Format.bool

instance Serialization String where format = Format.string

instance Units.Metadata units => Serialization (Qty units) where
  format =
    Format.coerce Format.float
      |> Format.title (Units.quantity @units)
      |> Format.description ("Units: " + Units.symbol @units)
      |> Format.examples [Qty.zero]
