module OpenSolid.Error
  ( Err (message)
  , IsZero (IsZero)
  , IsDegenerate (IsDegenerate)
  )
where

import Data.Text (Text)
import Data.Text qualified
import Prelude (Eq, Show, String, id, (.))
import Prelude qualified

class Show x => Err x where
  message :: x -> Text
  message = Data.Text.pack . Prelude.show

instance Err String where
  message = Data.Text.pack

instance Err Text where
  message = id

data IsDegenerate = IsDegenerate deriving (Eq, Show, Err)

data IsZero = IsZero deriving (Eq, Show, Err)
