module Curve3d.Internal
  ( Curve3d (..)
  , Interface (..)
  , startPoint
  , endPoint
  , pointOn
  , segmentBounds
  , derivative
  , reverse
  , bounds
  )
where

import Angle qualified
import Bounds3d (Bounds3d)
import Bounds3d qualified
import OpenSolid
import Point3d (Point3d)
import Point3d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Units qualified
import Vector3d (Vector3d)
import VectorCurve3d (VectorCurve3d)
import VectorCurve3d qualified

data Curve3d (coordinateSystem :: CoordinateSystem) where
  Constant ::
    Point3d (space @ units) ->
    Curve3d (space @ units)
  Line ::
    Point3d (space @ units) ->
    Point3d (space @ units) ->
    Curve3d (space @ units)
  Arc ::
    Point3d (space @ units) ->
    Vector3d (space @ units) ->
    Vector3d (space @ units) ->
    Angle ->
    Angle ->
    Curve3d (space @ units)
  Curve ::
    Interface curve (space @ units) =>
    curve ->
    Curve3d (space @ units)
  Coerce ::
    Curve3d (space @ units1) ->
    Curve3d (space @ units2)

deriving instance Show (Curve3d (space @ units))

instance HasUnits (Curve3d (space @ units)) where
  type Units (Curve3d (space @ units)) = units
  type Erase (Curve3d (space @ units)) = Curve3d (space @ Unitless)

instance
  space1 ~ space2 =>
  Units.Coercion (Curve3d (space1 @ unitsA)) (Curve3d (space2 @ unitsB))
  where
  coerce curve = case curve of
    Line p1 p2 -> Line (Units.coerce p1) (Units.coerce p2)
    Arc p0 vx vy a b -> Arc (Units.coerce p0) (Units.coerce vx) (Units.coerce vy) a b
    Coerce c -> Coerce c
    _ -> Coerce curve

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  startPointImpl :: curve -> Point3d coordinateSystem
  endPointImpl :: curve -> Point3d coordinateSystem
  pointOnImpl :: curve -> Float -> Point3d coordinateSystem
  segmentBoundsImpl :: curve -> Range Unitless -> Bounds3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  reverseImpl :: curve -> curve
  boundsImpl :: curve -> Bounds3d coordinateSystem

instance Interface (Curve3d (space @ units)) (space @ units) where
  startPointImpl = startPoint
  endPointImpl = endPoint
  pointOnImpl = pointOn
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  reverseImpl = reverse
  boundsImpl = bounds

startPoint :: Curve3d (space @ units) -> Point3d (space @ units)
startPoint curve = case curve of
  Constant p -> p
  Line p1 _ -> p1
  Arc{} -> pointOn curve 0.0
  Curve c -> startPointImpl c
  Coerce c -> Units.coerce (startPoint c)

endPoint :: Curve3d (space @ units) -> Point3d (space @ units)
endPoint curve = case curve of
  Constant p -> p
  Line _ p2 -> p2
  Arc{} -> pointOn curve 1.0
  Curve c -> endPointImpl c
  Coerce c -> Units.coerce (endPoint c)

pointOn :: Curve3d (space @ units) -> Float -> Point3d (space @ units)
pointOn curve t = case curve of
  Constant p -> p
  Line p1 p2 -> Point3d.interpolateFrom p1 p2 t
  Arc p0 v1 v2 a b -> do
    let theta = Qty.interpolateFrom a b t
    p0 + v1 * Angle.cos theta + v2 * Angle.sin theta
  Curve c -> pointOnImpl c t
  Coerce c -> Units.coerce (pointOn c t)

segmentBounds :: Curve3d (space @ units) -> Range Unitless -> Bounds3d (space @ units)
segmentBounds curve t = case curve of
  Constant p -> Bounds3d.constant p
  Line p1 p2 -> do
    let (t1, t2) = Range.endpoints t
    Bounds3d.hull2 (Point3d.interpolateFrom p1 p2 t1) (Point3d.interpolateFrom p1 p2 t2)
  Arc p0 v1 v2 a b -> do
    let theta = a + (b - a) * t
    p0 + Range.cos theta * v1 + Range.sin theta * v2
  Curve c -> segmentBoundsImpl c t
  Coerce c -> Units.coerce (segmentBounds c t)

derivative :: Curve3d (space @ units) -> VectorCurve3d (space @ units)
derivative curve = case curve of
  Constant _ -> VectorCurve3d.zero
  Line p1 p2 -> VectorCurve3d.constant (p2 - p1)
  Arc _ vx vy a b -> VectorCurve3d.derivative (VectorCurve3d.arc vx vy a b)
  Curve c -> derivativeImpl c
  Coerce c -> Units.coerce (derivative c)

reverse :: Curve3d (space @ units) -> Curve3d (space @ units)
reverse curve = case curve of
  Constant _ -> curve
  Line p1 p2 -> Line p2 p1
  Arc p0 vx vy a b -> Arc p0 vx vy b a
  Curve c -> Curve (reverseImpl c)
  Coerce c -> Units.coerce (reverse c)

bounds :: Curve3d (space @ units) -> Bounds3d (space @ units)
bounds curve = case curve of
  Constant p -> Bounds3d.constant p
  Line p1 p2 -> Bounds3d.hull2 p1 p2
  Arc{} -> segmentBounds curve Range.unit
  Curve c -> boundsImpl c
  Coerce c -> Units.coerce (bounds c)
