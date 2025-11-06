module OpenSolid.Error
  ( Message (message)
  , addContext
  )
where

import Data.Text (Text)
import OpenSolid.List (List)
import {-# SOURCE #-} OpenSolid.Text qualified as Text
import Prelude (Char, Show, id, (<>))

class Show error => Message error where
  message :: error -> Text
  message = Text.show

instance Message (List Char) where
  message = Text.pack

instance Message Text where
  message = id

addContext :: Text -> Text -> Text
addContext context "" = context
addContext context existing = context <> ":\n" <> Text.indent "  " existing
