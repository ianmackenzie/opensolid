module Line3d (Line3d (..)) where

import BoundingBox3d qualified
import Curve3d (IsCurve3d (..))
import Point3d (Point3d)
import Point3d qualified

data Line3d units coordinates
  = Line3d (Point3d units coordinates) (Point3d units coordinates)

instance IsCurve3d (Line3d units coordinates) units coordinates where
  startPoint (Line3d p1 _) = p1

  endPoint (Line3d _ p2) = p2

  pointOn (Line3d p1 p2) = Point3d.interpolateFrom p1 p2

  reverse (Line3d p1 p2) = Line3d p2 p1

  bisect (Line3d p1 p2) =
    let midpoint = Point3d.midpoint p1 p2
     in (Line3d p1 midpoint, Line3d midpoint p2)

  boundingBox (Line3d p1 p2) = BoundingBox3d.hull2 p1 p2
