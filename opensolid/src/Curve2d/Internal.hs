{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Curve2d.Internal
  ( Curve2d (..)
  , Interface (..)
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , derivative
  , reverse
  , bounds
  , transformBy
  )
where

import Angle qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Frame2d (Frame2d)
import List qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d (VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

data Curve2d (coordinateSystem :: CoordinateSystem) where
  Line ::
    Point2d (space @ units) ->
    Point2d (space @ units) ->
    Curve2d (space @ units)
  Arc ::
    Point2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Angle ->
    Angle ->
    Curve2d (space @ units)
  Curve ::
    Interface curve (space @ units) =>
    curve ->
    Curve2d (space @ units)
  Coerce ::
    Curve2d (space @ units1) ->
    Curve2d (space @ units2)
  PlaceIn ::
    Frame2d (global @ units) (Defines local) ->
    Curve2d (local @ units) ->
    Curve2d (global @ units)

deriving instance Show (Curve2d (space @ units))

instance HasUnits (Curve2d (space @ units)) where
  type Units (Curve2d (space @ units)) = units
  type Erase (Curve2d (space @ units)) = Curve2d (space @ Unitless)

instance
  space ~ space_ =>
  Units.Coercion (Curve2d (space @ units1)) (Curve2d (space_ @ units2))
  where
  coerce (Line p1 p2) = Line (Units.coerce p1) (Units.coerce p2)
  coerce (Arc centerPoint xVector yVector startAngle endAngle) =
    Arc (Units.coerce centerPoint) (Units.coerce xVector) (Units.coerce yVector) startAngle endAngle
  coerce (Coerce c) = Coerce c
  coerce c = Coerce c

instance Interface (Curve2d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  boundsImpl = bounds
  transformByImpl = transformBy

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Intersects (Curve2d (space @ units)) (Point2d (space_ @ units_)) units
  where
  curve ^ point = Range.any (segmentIsCoincidentWithPoint point curve) Range.unit

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Intersects (Point2d (space @ units)) (Curve2d (space_ @ units_)) units
  where
  point ^ curve = curve ^ point

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  ApproximateEquality (Curve2d (space @ units)) (Curve2d (space_ @ units_)) units
  where
  curve1 ~= curve2 =
    List.allTrue [pointOn curve1 t ~= pointOn curve2 t | t <- Range.samples Range.unit]

segmentIsCoincidentWithPoint ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Range Unitless ->
  Fuzzy Bool
segmentIsCoincidentWithPoint point curve domain = do
  let candidateBounds = segmentBounds curve domain
  if
    | not (point ^ candidateBounds) -> Resolved False
    | candidateBounds ~= point -> Resolved True
    | otherwise -> Unresolved

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point2d coordinateSystem
  endPointImpl :: curve -> Point2d coordinateSystem
  pointOnImpl :: curve -> Float -> Point2d coordinateSystem
  segmentBoundsImpl :: curve -> Range Unitless -> Bounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  reverseImpl :: curve -> curve
  boundsImpl :: curve -> Bounds2d coordinateSystem
  transformByImpl :: Transform2d tag coordinateSystem -> curve -> Curve2d coordinateSystem

startPoint :: Curve2d (space @ units) -> Point2d (space @ units)
startPoint (Line p1 _) = p1
startPoint arc@(Arc{}) = pointOn arc 0.0
startPoint (Curve curve) = startPointImpl curve
startPoint (Coerce curve) = Units.coerce (startPoint curve)
startPoint (PlaceIn frame curve) = Point2d.placeIn frame (startPoint curve)

endPoint :: Curve2d (space @ units) -> Point2d (space @ units)
endPoint (Line _ p2) = p2
endPoint arc@(Arc{}) = pointOn arc 1.0
endPoint (Curve curve) = endPointImpl curve
endPoint (Coerce curve) = Units.coerce (endPoint curve)
endPoint (PlaceIn frame curve) = Point2d.placeIn frame (endPoint curve)

pointOn :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
pointOn (Line p1 p2) t = Point2d.interpolateFrom p1 p2 t
pointOn (Arc p0 v1 v2 a b) t = do
  let theta = Qty.interpolateFrom a b t
  p0 + v1 * Angle.cos theta + v2 * Angle.sin theta
pointOn (Curve curve) t = pointOnImpl curve t
pointOn (Coerce curve) t = Units.coerce (pointOn curve t)
pointOn (PlaceIn frame curve) t = Point2d.placeIn frame (pointOn curve t)

segmentBounds :: Curve2d (space @ units) -> Range Unitless -> Bounds2d (space @ units)
segmentBounds (Line p1 p2) (Range t1 t2) =
  Bounds2d.hull2 (Point2d.interpolateFrom p1 p2 t1) (Point2d.interpolateFrom p1 p2 t2)
segmentBounds (Arc p0 v1 v2 a b) t = do
  let theta = a + (b - a) * t
  p0 + Range.cos theta * v1 + Range.sin theta * v2
segmentBounds (Curve curve) t = segmentBoundsImpl curve t
segmentBounds (Coerce curve) t = Units.coerce (segmentBounds curve t)
segmentBounds (PlaceIn frame curve) t = Bounds2d.placeIn frame (segmentBounds curve t)

derivative :: Curve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (Line p1 p2) = VectorCurve2d.constant (p2 - p1)
derivative (Arc _ v1 v2 a b) = VectorCurve2d.derivative (VectorCurve2d.arc v1 v2 a b)
derivative (Curve curve) = derivativeImpl curve
derivative (Coerce curve) = Units.coerce (derivative curve)
derivative (PlaceIn frame curve) = VectorCurve2d.placeIn frame (derivative curve)

reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
reverse (Line p1 p2) = Line p2 p1
reverse (Arc centerPoint xVector yVector startAngle endAngle) =
  Arc centerPoint xVector yVector endAngle startAngle
reverse (Curve curve) = Curve (reverseImpl curve)
reverse (Coerce curve) = Units.coerce (reverse curve)
reverse (PlaceIn frame curve) = PlaceIn frame (reverse curve)

bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
bounds (Line p1 p2) = Bounds2d.hull2 p1 p2
bounds arc@(Arc{}) = segmentBounds arc Range.unit
bounds (Curve curve) = boundsImpl curve
bounds (Coerce curve) = Units.coerce (bounds curve)
bounds (PlaceIn frame curve) = Bounds2d.placeIn frame (bounds curve)

transformBy :: Transform2d tag (space @ units) -> Curve2d (space @ units) -> Curve2d (space @ units)
transformBy transform (Line p1 p2) =
  Line (Point2d.transformBy transform p1) (Point2d.transformBy transform p2)
transformBy transform (Arc centerPoint xVector yVector startAngle endAngle) =
  Arc
    (Point2d.transformBy transform centerPoint)
    (Vector2d.transformBy transform xVector)
    (Vector2d.transformBy transform yVector)
    startAngle
    endAngle
transformBy transform (Curve curve) = Curve (transformByImpl transform curve)
transformBy transform (Coerce curve) = Units.coerce (transformBy (Units.coerce transform) curve)
transformBy transform (PlaceIn frame curve) =
  PlaceIn frame (transformBy (Transform2d.relativeTo frame transform) curve)

data PointCurveDifference (coordinateSystem :: CoordinateSystem)
  = PointCurveDifference (Point2d coordinateSystem) (Curve2d coordinateSystem)

deriving instance Show (PointCurveDifference (space @ units))

instance VectorCurve2d.Interface (PointCurveDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (PointCurveDifference point curve) = point - pointOn curve t
  segmentBoundsImpl t (PointCurveDifference point curve) = point - segmentBounds curve t
  derivativeImpl (PointCurveDifference _ curve) = -(derivative curve)
  transformByImpl transform (PointCurveDifference point curve) =
    VectorCurve2d.wrap $
      PointCurveDifference
        -- Note the slight hack here:
        -- the definition of VectorCurve2d.Interface states that the units of the transform
        -- do *not* have to match the units of the vector curve,
        -- because vectors and vector curves ignore translation
        -- (and the units of the transform are just the units of its translation part).
        -- This would in general mean that we couldn't apply the given transform to a Point2d or Curve2d,
        -- but in this case it's safe since any translation will cancel out
        -- when the point and curve are subtracted from each other.
        (Point2d.transformBy (Units.coerce transform) point)
        (transformBy (Units.coerce transform) curve)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point2d (space @ units))
    (Curve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference (coordinateSystem :: CoordinateSystem)
  = CurvePointDifference (Curve2d coordinateSystem) (Point2d coordinateSystem)

deriving instance Show (CurvePointDifference (space @ units))

instance VectorCurve2d.Interface (CurvePointDifference (space @ units)) (space @ units) where
  evaluateAtImpl t (CurvePointDifference curve point) = pointOn curve t - point
  segmentBoundsImpl t (CurvePointDifference curve point) = segmentBounds curve t - point
  derivativeImpl (CurvePointDifference curve _) = derivative curve
  transformByImpl transform (CurvePointDifference curve point) =
    VectorCurve2d.wrap $
      CurvePointDifference
        -- Note the same slight hack here as described in PointCurveDifference above
        (transformBy (Units.coerce transform) curve)
        (Point2d.transformBy (Units.coerce transform) point)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Curve2d (space @ units))
    (Point2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  curve - point = VectorCurve2d (CurvePointDifference curve point)
