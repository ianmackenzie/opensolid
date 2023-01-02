module QuadraticSpline3d (QuadraticSpline3d (..)) where

import BoundingBox3d qualified
import Curve3d (IsCurve3d (..))
import Point3d (Point3d)
import Point3d qualified

data QuadraticSpline3d coordinates
    = QuadraticSpline3d
        !(Point3d coordinates)
        !(Point3d coordinates)
        !(Point3d coordinates)

instance IsCurve3d QuadraticSpline3d where
    startPoint (QuadraticSpline3d p1 _ _) = p1

    endPoint (QuadraticSpline3d _ _ p3) = p3

    pointOn (QuadraticSpline3d p1 p2 p3) t =
        let q1 = Point3d.interpolateFrom p1 p2 t
            q2 = Point3d.interpolateFrom p2 p3 t
         in Point3d.interpolateFrom q1 q2 t

    reverse (QuadraticSpline3d p1 p2 p3) = QuadraticSpline3d p3 p2 p1

    bisect (QuadraticSpline3d p1 p2 p3) =
        let q1 = Point3d.midpoint p1 p2
            q2 = Point3d.midpoint p2 p3
            r = Point3d.midpoint q1 q2
         in (QuadraticSpline3d p1 q1 r, QuadraticSpline3d r q2 p3)

    boundingBox (QuadraticSpline3d p1 p2 p3) = BoundingBox3d.hull3 p1 p2 p3
