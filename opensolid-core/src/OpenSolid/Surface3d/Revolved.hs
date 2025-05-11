module OpenSolid.Surface3d.Revolved (Error (..)) where

import OpenSolid.Error qualified as Error
import OpenSolid.Prelude

data Error
  = ProfileIsOnAxis
  | ProfileCrossesAxis
  deriving (Eq, Show, Error.Message)
