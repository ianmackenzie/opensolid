module OpenSolid.VectorCurve3d.Zeros (Error (..)) where

import OpenSolid.Error qualified as Error
import OpenSolid.Prelude

data Error
  = HigherOrderZero
  | ZeroEverywhere
  deriving (Eq, Show, Error.Message)
