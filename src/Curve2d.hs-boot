module Curve2d
  ( Curve2d
  , IsCurve2d
  , DegenerateCurve
  , from
  , startPoint
  , endPoint
  , evaluateAt
  , segmentBounds
  , derivative
  )
where

import BoundingBox2d (BoundingBox2d)
import Domain (Domain)
import OpenSolid
import Point2d (Point2d)
import VectorCurve2d (VectorCurve2d)

class Show curve => IsCurve2d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  evaluateAtImpl :: Float -> curve -> Point2d coordinateSystem
  segmentBoundsImpl :: Domain -> curve -> BoundingBox2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d coordinateSystem

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem)

data DegenerateCurve

from
  :: (Tolerance units, IsCurve2d curve (space @ units))
  => curve
  -> Result DegenerateCurve (Curve2d (space @ units))
startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
