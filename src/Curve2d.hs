module Curve2d (
    Curve2d (Curve2d),
    IsCurve2d (..),
    constant,
) where

import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import OpenSolid
import Point2d (Point2d)
import Range (Range)
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

class IsCurve2d curve where
    startPoint :: curve coordinates -> Point2d coordinates
    endPoint :: curve coordinates -> Point2d coordinates
    pointOn :: curve coordinates -> Float -> Point2d coordinates
    segmentBounds :: curve coordinates -> Range Unitless -> BoundingBox2d coordinates
    derivative :: curve coordinates -> VectorCurve2d Meters coordinates
    reverse :: curve coordinates -> curve coordinates
    bisect :: curve coordinates -> (curve coordinates, curve coordinates)
    boundingBox :: curve coordinates -> BoundingBox2d coordinates

data Curve2d coordinates = forall curve. IsCurve2d curve => Curve2d (curve coordinates)

instance IsCurve2d Curve2d where
    startPoint (Curve2d curve) = startPoint curve
    endPoint (Curve2d curve) = endPoint curve
    pointOn (Curve2d curve) t = pointOn curve t
    segmentBounds (Curve2d curve) t = segmentBounds curve t
    derivative (Curve2d curve) = derivative curve
    reverse (Curve2d curve) = Curve2d (reverse curve)
    bisect (Curve2d curve) =
        let (curve1, curve2) = bisect curve
         in (Curve2d curve1, Curve2d curve2)
    boundingBox (Curve2d curve) = boundingBox curve

instance IsCurve2d Point2d where
    startPoint = identity
    endPoint = identity
    pointOn = always
    segmentBounds point _ = BoundingBox2d.constant point
    derivative _ = VectorCurve2d.zero
    reverse = identity
    bisect point = (point, point)
    boundingBox = BoundingBox2d.constant

constant :: Point2d coordinates -> Curve2d coordinates
constant = Curve2d
