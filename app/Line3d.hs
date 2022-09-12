module Line3d (Line3d (..)) where

import qualified BoundingBox3d
import Curve3d (IsCurve3d (..))
import OpenSolid
import Point3d (Point3d)
import qualified Point3d

data Line3d coordinates
    = Line3d (Point3d coordinates) (Point3d coordinates)

instance IsCurve3d Line3d where
    startPoint (Line3d p1 _) =
        p1

    endPoint (Line3d _ p2) =
        p2

    pointOn (Line3d p1 p2) t =
        Point3d.interpolateFrom p1 p2 t

    reverse (Line3d p1 p2) =
        Line3d p2 p1

    bisect (Line3d p1 p2) =
        let midpoint = Point3d.midpoint p1 p2
         in (Line3d p1 midpoint, Line3d midpoint p2)

    boundingBox (Line3d p1 p2) =
        BoundingBox3d.hull2 p1 p2
