module OpenSolid.IndexOutOfBounds (IndexOutOfBounds (..)) where

import OpenSolid.Prelude

data IndexOutOfBounds = IndexOutOfBounds
  { index :: Int
  , size :: Int
  }
  deriving (Show, Exception)
