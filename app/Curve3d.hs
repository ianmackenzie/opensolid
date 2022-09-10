module Curve3d (
    Curve3d (..),
    IsCurve3d (..),
) where

import BoundingBox3d (BoundingBox3d)
import OpenSolid
import Point3d (Point3d)

class IsCurve3d curve where
    startPoint :: curve coordinates -> Point3d coordinates
    endPoint :: curve coordinates -> Point3d coordinates
    pointOn :: curve coordinates -> Float -> Point3d coordinates
    reverse :: curve coordinates -> curve coordinates
    bisect :: curve coordinates -> (curve coordinates, curve coordinates)
    boundingBox :: curve coordinates -> BoundingBox3d coordinates

data Curve3d coordinates = forall curve. IsCurve3d curve => Curve3d (curve coordinates)

instance IsCurve3d Curve3d where
    startPoint (Curve3d curve) =
        startPoint curve

    endPoint (Curve3d curve) =
        endPoint curve

    pointOn (Curve3d curve) t =
        pointOn curve t

    reverse (Curve3d curve) =
        Curve3d (reverse curve)

    bisect (Curve3d curve) =
        let (curve1, curve2) = bisect curve
         in (Curve3d curve1, Curve3d curve2)

    boundingBox (Curve3d curve) =
        boundingBox curve
