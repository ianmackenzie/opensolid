module Curve2d
  ( Curve2d
  , DegenerateCurve
  , startPoint
  , endPoint
  , evaluateAt
  , segmentBounds
  , derivative
  , tangentBounds
  )
where

import BoundingBox2d (BoundingBox2d)
import Curve2d.Internal qualified as Internal
import Domain (Domain)
import OpenSolid
import Point2d (Point2d)
import VectorBox2d (VectorBox2d)
import VectorCurve2d (VectorCurve2d)

type Curve2d (coordinateSystem :: CoordinateSystem) = Internal.Curve2d coordinateSystem

data DegenerateCurve

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
segmentBounds :: Domain -> Curve2d (space @ units) -> BoundingBox2d (space @ units)
derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
tangentBounds :: Domain -> Curve2d (space @ units) -> VectorBox2d (space @ Unitless)
