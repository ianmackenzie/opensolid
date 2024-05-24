module Text.Parse (int, float) where

import Arithmetic
import Basics
import Composition
import Data.Text qualified
import Data.Text.Read (Reader)
import Data.Text.Read qualified
import {-# SOURCE #-} Float (Float)
import {-# SOURCE #-} Qty (Qty (Qty_))
import Result (Result (Error, Ok))
import Result qualified
import Prelude qualified

num :: Prelude.Num a => Reader a -> Text -> Result String a
num reader text =
  case Data.Text.Read.signed reader text of
    Prelude.Right (value, suffix)
      | Data.Text.null suffix -> Ok value
      | otherwise -> do
          let message =
                "Could not parse '"
                  + Data.Text.unpack text
                  + "' as a number - has extra trailing text '"
                  + Data.Text.unpack suffix
                  + "'"
          Error message
    Prelude.Left message -> Error message

int :: Text -> Result String Int
int = num Data.Text.Read.decimal

float :: Text -> Result String Float
float = num Data.Text.Read.double >> Result.map Qty_
