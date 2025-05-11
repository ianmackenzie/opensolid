module OpenSolid.Curve2d.FindPoint (Error (..)) where

import OpenSolid.Error qualified as Error
import OpenSolid.Prelude

data Error = CurveIsCoincidentWithPoint deriving (Eq, Show, Error.Message)
