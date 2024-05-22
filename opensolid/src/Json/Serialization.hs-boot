module Json.Serialization (Serialization (..)) where

import {-# SOURCE #-} Json.Format (Format)

class Serialization a where
  format :: Format a
