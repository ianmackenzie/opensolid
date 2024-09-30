module Line2d
  ( from
  , Line2d
  , startPoint
  , endPoint
  , placeIn
  )
where

import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Data.Coerce qualified
import Frame2d (Frame2d)
import Jit.Expression qualified as Expression
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Range qualified
import Units qualified
import VectorCurve2d qualified

from ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Curve2d (space @ units)
from p1 p2 = Curve2d.new (Line2d p1 p2)

type role Line2d phantom

data Line2d (coordinateSystem :: CoordinateSystem) = Line2d
  { startPoint :: Point2d coordinateSystem
  , endPoint :: Point2d coordinateSystem
  }

deriving instance Show (Line2d (space @ units))

instance HasUnits (Line2d (space @ units)) where
  type UnitsOf (Line2d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Line2d (space1 @ unitsA)) (Line2d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance Curve2d.Interface (Line2d (space @ units)) (space @ units) where
  startPointImpl (Line2d p1 _) = p1
  endPointImpl (Line2d _ p2) = p2
  pointOnImpl (Line2d p1 p2) t = Point2d.interpolateFrom p1 p2 t
  segmentBoundsImpl (Line2d p1 p2) t = do
    let (t1, t2) = Range.endpoints t
    Bounds2d.hull2 (Point2d.interpolateFrom p1 p2 t1) (Point2d.interpolateFrom p1 p2 t2)
  derivativeImpl (Line2d p1 p2) = VectorCurve2d.constant (p2 - p1)
  reverseImpl (Line2d p1 p2) = Line2d p2 p1
  boundsImpl (Line2d p1 p2) = Bounds2d.hull2 p1 p2
  transformByImpl transform (Line2d p1 p2) =
    Curve2d.new (Line2d (Point2d.transformBy transform p1) (Point2d.transformBy transform p2))
  asLineImpl line = Just line
  toAstImpl (Line2d p1 p2) = Just (p1 + Expression.parameter * (p2 - p1))

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Line2d (local @ units) ->
  Line2d (global @ units)
placeIn frame (Line2d p1 p2) = Line2d (Point2d.placeIn frame p1) (Point2d.placeIn frame p2)
