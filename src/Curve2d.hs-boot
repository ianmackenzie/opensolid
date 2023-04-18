module Curve2d
  ( Curve2d (Curve2d)
  , IsCurve2d
  )
where

import Angle (Angle)
import BoundingBox2d (BoundingBox2d)
import OpenSolid
import Point2d (Point2d)
import Range (Range)
import Units (Unitless)
import VectorCurve2d (VectorCurve2d)

class Show curve => IsCurve2d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  evaluateImpl :: curve -> Float -> Point2d coordinateSystem
  segmentBoundsImpl :: curve -> Range Unitless -> BoundingBox2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  bisectImpl :: curve -> (curve, curve)
  boundingBoxImpl :: curve -> BoundingBox2d coordinateSystem

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
  Arc :: Point2d (space @ units) -> Qty units -> Angle -> Angle -> Curve2d (space @ units)
  Curve2d :: IsCurve2d curve (space @ units) => curve -> Curve2d (space @ units)
