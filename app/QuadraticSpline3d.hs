module QuadraticSpline3d (QuadraticSpline3d (..)) where

import Curve3d (IsCurve3d (..))
import Point3d (Point3d)
import qualified Point3d

data QuadraticSpline3d coordinates
    = QuadraticSpline3d
        (Point3d coordinates)
        (Point3d coordinates)
        (Point3d coordinates)

instance IsCurve3d QuadraticSpline3d where
    startPoint (QuadraticSpline3d p1 _ _) =
        p1

    endPoint (QuadraticSpline3d _ _ p3) =
        p3

    pointOn (QuadraticSpline3d p1 p2 p3) t =
        let q1 = Point3d.interpolateFrom p1 p2 t
            q2 = Point3d.interpolateFrom p2 p3 t
         in Point3d.interpolateFrom q1 q2 t

    bisect (QuadraticSpline3d p1 p2 p3) =
        let q1 = Point3d.midpoint p1 p2
            q2 = Point3d.midpoint p2 p3
            r = Point3d.midpoint q1 q2
         in (QuadraticSpline3d p1 q1 r, QuadraticSpline3d r q2 p3)
