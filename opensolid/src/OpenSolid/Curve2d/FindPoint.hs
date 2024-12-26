module OpenSolid.Curve2d.FindPoint (Error (..)) where

import Error qualified
import OpenSolid.Prelude

data Error
  = CurveIsCoincidentWithPoint
  | HigherOrderSolution
  deriving (Eq, Show, Error.Message)
