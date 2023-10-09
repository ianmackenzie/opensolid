module Curve3d
  ( Curve3d (..)
  , IsCurve3d (..)
  , DegenerateCurve (DegenerateCurve)
  )
where

import Bounds3d (Bounds3d)
import Bounds3d qualified
import Domain (Domain)
import OpenSolid
import Point3d (Point3d)
import VectorCurve3d (VectorCurve3d)
import VectorCurve3d qualified

class IsCurve3d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  startPointImpl :: curve -> Point3d coordinateSystem
  endPointImpl :: curve -> Point3d coordinateSystem
  evaluateImpl :: curve -> Float -> Point3d coordinateSystem
  segmentBoundsImpl :: curve -> Domain -> Bounds3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  reverseImpl :: curve -> curve
  boundsImpl :: curve -> Bounds3d coordinateSystem

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Curve3d :: (IsCurve3d curve (space @ units)) => curve -> Curve3d (space @ units)

instance IsCurve3d (Point3d (space @ units)) (space @ units) where
  startPointImpl = identity
  endPointImpl = identity
  evaluateImpl point _ = point
  segmentBoundsImpl point _ = Bounds3d.constant point
  derivativeImpl _ = VectorCurve3d.zero
  reverseImpl = identity
  boundsImpl = Bounds3d.constant

instance IsCurve3d (Curve3d (space @ units)) (space @ units) where
  startPointImpl (Curve3d curve) = startPointImpl curve
  endPointImpl (Curve3d curve) = endPointImpl curve
  evaluateImpl (Curve3d curve) = evaluateImpl curve
  segmentBoundsImpl (Curve3d curve) = segmentBoundsImpl curve
  derivativeImpl (Curve3d curve) = derivativeImpl curve
  reverseImpl (Curve3d curve) = Curve3d (reverseImpl curve)
  boundsImpl (Curve3d curve) = boundsImpl curve

data DegenerateCurve = DegenerateCurve deriving (Eq, Show, ErrorMessage)
