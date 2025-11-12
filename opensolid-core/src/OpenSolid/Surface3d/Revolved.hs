module OpenSolid.Surface3d.Revolved (Error (..)) where

import OpenSolid.Prelude

data Error
  = ProfileIsOnAxis
  | ProfileCrossesAxis
  deriving (Eq, Show)
