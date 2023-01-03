module Line2d (Line2d (..)) where

import BoundingBox2d qualified
import Curve2d (Curve2d (Curve2d), IsCurve2d (..))
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

data Line2d coordinates
    = Line2d (Point2d Meters coordinates) (Point2d Meters coordinates)

instance IsCurve2d Line2d where
    startPoint (Line2d p1 _) = p1

    endPoint (Line2d _ p2) = p2

    pointOn (Line2d p1 p2) = Point2d.interpolateFrom p1 p2

    segmentBounds line t =
        BoundingBox2d.hull2
            (pointOn line (Range.minValue t))
            (pointOn line (Range.maxValue t))

    derivative (Line2d p1 p2) = VectorCurve2d.constant (p2 - p1)

    reverse (Line2d p1 p2) = Line2d p2 p1

    bisect (Line2d p1 p2) =
        let midpoint = Point2d.midpoint p1 p2
         in (Line2d p1 midpoint, Line2d midpoint p2)

    boundingBox (Line2d p1 p2) = BoundingBox2d.hull2 p1 p2

instance Subtraction Line2d (Point2d Meters) (VectorCurve2d Meters) where
    line - point = Curve2d line - point

instance Subtraction (Point2d Meters) Line2d (VectorCurve2d Meters) where
    point - line = point - Curve2d line
