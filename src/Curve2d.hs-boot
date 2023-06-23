module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d
  , derivative
  , segmentBounds
  )
where

import Angle (Angle)
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

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
  Arc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
  Curve2d :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
