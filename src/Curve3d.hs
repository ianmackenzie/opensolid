module Curve3d
  ( Curve3d (..)
  , IsCurve3d (..)
  , DegenerateCurve (DegenerateCurve)
  )
where

import BoundingBox3d (BoundingBox3d)
import BoundingBox3d qualified
import Domain (Domain)
import OpenSolid
import Point3d (Point3d)
import VectorCurve3d (VectorCurve3d)
import VectorCurve3d qualified

class IsCurve3d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  startPointImpl :: curve -> Point3d coordinateSystem
  endPointImpl :: curve -> Point3d coordinateSystem
  evaluateImpl :: curve -> Float -> Point3d coordinateSystem
  segmentBoundsImpl :: curve -> Domain -> BoundingBox3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox3d coordinateSystem

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve3d :: IsCurve3d curve (space @ units) => curve -> Curve3d (space @ units)

instance IsCurve3d (Point3d (space @ units)) (space @ units) where
  startPointImpl = identity
  endPointImpl = identity
  evaluateImpl point _ = point
  segmentBoundsImpl point _ = BoundingBox3d.constant point
  derivativeImpl _ = VectorCurve3d.zero
  reverseImpl = identity
  bisectImpl point = (point, point)
  boundingBoxImpl = BoundingBox3d.constant

instance IsCurve3d (Curve3d (space @ units)) (space @ units) where
  startPointImpl (Curve3d curve) = startPointImpl curve
  endPointImpl (Curve3d curve) = endPointImpl curve
  evaluateImpl (Curve3d curve) = evaluateImpl curve
  segmentBoundsImpl (Curve3d curve) = segmentBoundsImpl curve
  derivativeImpl (Curve3d curve) = derivativeImpl curve
  reverseImpl (Curve3d curve) = Curve3d (reverseImpl curve)
  bisectImpl (Curve3d curve) =
    let (curve1, curve2) = bisectImpl curve
     in (Curve3d curve1, Curve3d curve2)
  boundingBoxImpl (Curve3d curve) = boundingBoxImpl curve

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, ErrorMessage)
