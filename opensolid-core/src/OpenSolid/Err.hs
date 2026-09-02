module OpenSolid.Err (Err (message)) where

import Data.Text (Text)
import Data.Text qualified
import Prelude (Show, String, id, (.))
import Prelude qualified

class Show x => Err x where
  message :: x -> Text
  message = Data.Text.pack . Prelude.show

instance Err String where
  message = Data.Text.pack

instance Err Text where
  message = id
