module OpenSolid.Surface3D.Revolved (Error (..)) where

import OpenSolid.Prelude

data Error
  = ProfileIsOnAxis
  | ProfileCrossesAxis
  deriving (Eq, Show)
