module OpenSolid.Text.Parse (int, float) where

import Data.Text qualified
import Data.Text.Read (Reader)
import Data.Text.Read qualified
import OpenSolid.Arithmetic
import OpenSolid.Bootstrap
import OpenSolid.Composition
import {-# SOURCE #-} OpenSolid.Float (Float)
import {-# SOURCE #-} OpenSolid.Float qualified as Float
import OpenSolid.Result (Result (Failure, Success))
import OpenSolid.Result qualified as Result
import Prelude qualified

num :: Prelude.Num a => Reader a -> Text -> Result Text a
num reader text =
  case Data.Text.Read.signed reader text of
    Prelude.Right (value, suffix)
      | Data.Text.null suffix -> Success value
      | otherwise -> do
          let message =
                "Could not parse '"
                  + text
                  + "' as a number - has extra trailing text '"
                  + suffix
                  + "'"
          Failure message
    Prelude.Left message -> Failure (Data.Text.pack message)

int :: Text -> Result Text Int
int = num Data.Text.Read.decimal

float :: Text -> Result Text Float
float = num Data.Text.Read.double >> Result.map Float.fromDouble
