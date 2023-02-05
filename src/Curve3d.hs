module Curve3d
  ( Curve3d (..)
  , IsCurve3d (..)
  )
where

import BoundingBox3d (BoundingBox3d)
import BoundingBox3d qualified
import OpenSolid
import Point3d (Point3d)

class IsCurve3d curve coordinates units | curve -> units, curve -> coordinates where
  startPoint :: curve -> Point3d coordinates units
  endPoint :: curve -> Point3d coordinates units
  pointOn :: curve -> Float -> Point3d coordinates units
  reverse :: curve -> curve
  bisect :: curve -> (curve, curve)
  boundingBox :: curve -> BoundingBox3d coordinates units

data Curve3d coordinates units = forall curve. IsCurve3d curve coordinates units => Curve3d curve

instance IsCurve3d (Point3d coordinates units) coordinates units where
  startPoint = identity
  endPoint = identity
  pointOn point _ = point
  reverse = identity
  bisect point = (point, point)
  boundingBox = BoundingBox3d.constant

instance IsCurve3d (Curve3d coordinates units) coordinates units where
  startPoint (Curve3d curve) = startPoint curve
  endPoint (Curve3d curve) = endPoint curve
  pointOn (Curve3d curve) = pointOn curve
  reverse (Curve3d curve) = Curve3d (reverse curve)
  bisect (Curve3d curve) =
    let (curve1, curve2) = bisect curve
     in (Curve3d curve1, Curve3d curve2)
  boundingBox (Curve3d curve) = boundingBox curve
