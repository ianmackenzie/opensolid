module OpenSolid.Curve2d.Intersections (Error (..)) where

import OpenSolid.Prelude

data Error
  = CurveHasDegeneracy
  | FirstCurveIsPoint
  | SecondCurveIsPoint
  deriving (Eq, Show)
