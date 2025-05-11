module OpenSolid.VectorCurve2d.Zeros (Error (..)) where

import OpenSolid.Error qualified as Error
import OpenSolid.Prelude

data Error = ZeroEverywhere deriving (Eq, Show, Error.Message)
