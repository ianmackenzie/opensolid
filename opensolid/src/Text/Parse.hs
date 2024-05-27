module Text.Parse (int, float) where

import Arithmetic
import Basics
import Composition
import Data.Text qualified
import Data.Text.Read (Reader)
import Data.Text.Read qualified
import {-# SOURCE #-} Float (Float)
import {-# SOURCE #-} Float qualified
import Result (Result (Error, Ok))
import Result qualified
import Prelude qualified

num :: Prelude.Num a => Reader a -> Text -> Result Text a
num reader text =
  case Data.Text.Read.signed reader text of
    Prelude.Right (value, suffix)
      | Data.Text.null suffix -> Ok value
      | otherwise -> do
          let message =
                "Could not parse '"
                  + text
                  + "' as a number - has extra trailing text '"
                  + suffix
                  + "'"
          Error message
    Prelude.Left message -> Error (Data.Text.pack message)

int :: Text -> Result Text Int
int = num Data.Text.Read.decimal

float :: Text -> Result Text Float
float = num Data.Text.Read.double >> Result.map Float.fromDouble
