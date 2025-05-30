module OpenSolid.Error
  ( Message (message)
  , addContext
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Text qualified as Text

class (Eq error, Show error) => Message error where
  message :: error -> Text
  message = Text.show

instance Message (List Char) where
  message = Text.pack

instance Message Text where
  message = identity

addContext :: Text -> Text -> Text
addContext context "" = context
addContext context existing = context <> ":\n" <> Text.indent "  " existing
