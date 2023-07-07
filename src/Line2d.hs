module Line2d
  ( from
  )
where

import Curve2d (Curve2d, DegenerateCurve (DegenerateCurve))
import Curve2d.Internal qualified
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)

from
  :: Tolerance units
  => Point2d (space @ units)
  -> Point2d (space @ units)
  -> Result DegenerateCurve (Curve2d (space @ units))
from p1 p2 =
  case Direction2d.from p1 p2 of
    Ok direction -> Ok (Curve2d.Internal.Line p1 p2 direction)
    Error Direction2d.PointsAreCoincident -> Error DegenerateCurve
