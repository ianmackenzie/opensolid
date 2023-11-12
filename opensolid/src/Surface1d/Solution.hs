module Surface1d.Solution (Solution (..)) where

import Curve2d (Curve2d)
import OpenSolid
import Point2d (Point2d)
import Uv qualified

data Solution
  = CrossingCurve (Curve2d Uv.Coordinates)
  | TangentCurve (Curve2d Uv.Coordinates) Sign
  | TangentPoint (Point2d Uv.Coordinates) Sign
  deriving (Show)
