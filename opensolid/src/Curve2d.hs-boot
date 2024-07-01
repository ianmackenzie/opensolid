module Curve2d
  ( Curve2d
  , DegenerateCurve (DegenerateCurve)
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , derivative
  , tangentDirection
  )
where

import Bounds2d (Bounds2d)
import Curve2d.Internal qualified as Internal
import DirectionCurve2d (DirectionCurve2d)
import OpenSolid
import Point2d (Point2d)
import Range (Range)
import VectorCurve2d (VectorCurve2d)

type Curve2d (coordinateSystem :: CoordinateSystem) = Internal.Curve2d coordinateSystem

data DegenerateCurve = DegenerateCurve

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
segmentBounds :: Curve2d (space @ units) -> Range Unitless -> Bounds2d (space @ units)
derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
tangentDirection :: Tolerance units => Curve2d (space @ units) -> Result DegenerateCurve (DirectionCurve2d space)
