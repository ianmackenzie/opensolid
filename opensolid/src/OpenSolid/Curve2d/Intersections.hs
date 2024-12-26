module OpenSolid.Curve2d.Intersections (Error (..)) where

import Error qualified
import OpenSolid.Prelude

data Error
  = CurveHasDegeneracy
  | HigherOrderIntersection
  | FirstCurveIsPoint
  | SecondCurveIsPoint
  deriving (Eq, Show, Error.Message)
