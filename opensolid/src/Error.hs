module Error
  ( Error (message)
  , addContext
  )
where

import Arithmetic
import Basics
import {-# SOURCE #-} Text qualified

class (Eq error, Show error) => Error error where
  message :: error -> Text
  message = Text.show

instance Error (List Char) where
  message = Text.pack

instance Error Text where
  message = identity

addContext :: Text -> Text -> Text
addContext string text = string + ":\n" + Text.indent "  " text
