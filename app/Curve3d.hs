module Curve3d (
    Curve3d (..),
    IsCurve3d (..),
) where

import Point3d (Point3d)

class IsCurve3d curve where
    startPoint :: curve coordinates -> Point3d coordinates
    endPoint :: curve coordinates -> Point3d coordinates

data Curve3d coordinates = forall curve. IsCurve3d curve => Curve3d (curve coordinates)

instance IsCurve3d Curve3d where
    startPoint (Curve3d curve) =
        startPoint curve

    endPoint (Curve3d curve) =
        endPoint curve
