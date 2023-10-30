module Surface1d.Solution (Solution (..)) where

import Curve2d (Curve2d)
import OpenSolid
import Uv qualified

data Solution
  = CrossingCurve (Curve2d Uv.Coordinates) Sign
  | TangentCurve Sign
  | TangentPoint Sign
