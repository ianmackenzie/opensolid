module Line3d
  ( from
  )
where

import Bounds3d qualified
import Curve3d (Curve3d (Curve3d), IsCurve3d (..))
import OpenSolid
import Point3d (Point3d)
import Point3d qualified
import Range (Range (Range))
import VectorCurve3d qualified

data Line3d (coordinateSystem :: CoordinateSystem)
  = Line3d (Point3d coordinateSystem) (Point3d coordinateSystem)

instance IsCurve3d (Line3d (space @ units)) (space @ units) where
  startPointImpl (Line3d p1 _) = p1

  endPointImpl (Line3d _ p2) = p2

  evaluateImpl (Line3d p1 p2) t = Point3d.interpolateFrom p1 p2 t

  segmentBoundsImpl (Line3d p1 p2) (Range tl th) =
    Bounds3d.hull2
      (Point3d.interpolateFrom p1 p2 tl)
      (Point3d.interpolateFrom p1 p2 th)

  derivativeImpl (Line3d p1 p2) = VectorCurve3d.constant (p2 - p1)

  reverseImpl (Line3d p1 p2) = Line3d p2 p1

  boundsImpl (Line3d p1 p2) = Bounds3d.hull2 p1 p2

from :: Point3d (space @ units) -> Point3d (space @ units) -> Curve3d (space @ units)
from p1 p2 = Curve3d (Line3d p1 p2)
