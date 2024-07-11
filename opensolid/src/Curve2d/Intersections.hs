module Curve2d.Intersections (Error (..)) where

import Error qualified
import OpenSolid

data Error
  = CurveHasDegeneracy
  | HigherOrderIntersection
  | FirstCurveIsPoint
  | SecondCurveIsPoint
  deriving (Eq, Show, Error.Message)
