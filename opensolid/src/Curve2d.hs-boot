module Curve2d
  ( Curve2d
  , DegenerateCurve
  , startPoint
  , endPoint
  , evaluateAt
  , segmentBounds
  , derivative
  , tangentDirection
  )
where

import Bounds2d (Bounds2d)
import Curve2d.Internal qualified as Internal
import DirectionCurve2d (DirectionCurve2d)
import OpenSolid
import Parameter qualified
import Point2d (Point2d)
import VectorCurve2d (VectorCurve2d)

type Curve2d (coordinateSystem :: CoordinateSystem) = Internal.Curve2d coordinateSystem

data DegenerateCurve

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
evaluateAt :: Float -> Curve2d (space @ units) -> Point2d (space @ units)
segmentBounds :: Parameter.Bounds -> Curve2d (space @ units) -> Bounds2d (space @ units)
derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
tangentDirection :: Curve2d (space @ units) -> DirectionCurve2d space
