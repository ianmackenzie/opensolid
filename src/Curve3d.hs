module Curve3d
  ( Curve3d (..)
  , IsCurve3d (..)
  )
where

import BoundingBox3d (BoundingBox3d)
import BoundingBox3d qualified
import OpenSolid
import Point3d (Point3d)

class IsCurve3d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  startPoint :: curve -> Point3d coordinateSystem
  endPoint :: curve -> Point3d coordinateSystem
  pointOn :: curve -> Float -> Point3d coordinateSystem
  reverse :: curve -> curve
  bisect :: curve -> (curve, curve)
  boundingBox :: curve -> BoundingBox3d coordinateSystem

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve3d :: IsCurve3d curve (space @ units) => curve -> Curve3d (space @ units)

instance IsCurve3d (Point3d (space @ units)) (space @ units) where
  startPoint = identity
  endPoint = identity
  pointOn point _ = point
  reverse = identity
  bisect point = (point, point)
  boundingBox = BoundingBox3d.constant

instance IsCurve3d (Curve3d (space @ units)) (space @ units) where
  startPoint (Curve3d curve) = startPoint curve
  endPoint (Curve3d curve) = endPoint curve
  pointOn (Curve3d curve) = pointOn curve
  reverse (Curve3d curve) = Curve3d (reverse curve)
  bisect (Curve3d curve) =
    let (curve1, curve2) = bisect curve
     in (Curve3d curve1, Curve3d curve2)
  boundingBox (Curve3d curve) = boundingBox curve
