module OpenSolid.Text.Parse (int, number) where

import Data.Text qualified
import Data.Text.Read (Reader)
import Data.Text.Read qualified
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import Prelude qualified

num :: Prelude.Num a => Reader a -> Text -> Result Text a
num reader text =
  case Data.Text.Read.signed reader text of
    Right (value, suffix)
      | Data.Text.null suffix -> Ok value
      | otherwise -> do
          let message =
                "Could not parse '"
                  <> text
                  <> "' as a number - has extra trailing text '"
                  <> suffix
                  <> "'"
          Error message
    Left message -> Error (Data.Text.pack message)

int :: Text -> Result Text Int
int = num Data.Text.Read.decimal

number :: Text -> Result Text Number
number = Result.map Quantity . num Data.Text.Read.double
