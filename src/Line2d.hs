module Line2d
  ( from
  )
where

import Curve2d (Curve2d, DegenerateCurve (DegenerateCurve))
import Curve2d qualified
import OpenSolid
import Point2d (Point2d)

from
  :: Tolerance units
  => Point2d (space @ units)
  -> Point2d (space @ units)
  -> Result DegenerateCurve (Curve2d (space @ units))
from p1 p2
  | p1 ~= p2 = Error DegenerateCurve
  | otherwise = Ok (Curve2d.unsafeLine p1 p2)
