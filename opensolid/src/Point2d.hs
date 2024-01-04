module Point2d
  ( Point2d (Point2d)
  , origin
  , x
  , y
  , xy
  , meters
  , centimeters
  , millimeters
  , inches
  , xCoordinate
  , yCoordinate
  , midpoint
  , interpolateFrom
  , distanceFrom
  , angleFrom
  , signedDistanceAlong
  , signedDistanceFrom
  , placeIn
  , relativeTo
  )
where

import {-# SOURCE #-} Axis2d (Axis2d)
import {-# SOURCE #-} Axis2d qualified
import Bounded qualified
import {-# SOURCE #-} Bounds2d (Bounds2d (Bounds2d))
import {-# SOURCE #-} Bounds2d qualified
import Direction2d (Direction2d (Direction2d))
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import Length qualified
import OpenSolid
import Qty qualified
import Units (Meters)
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Point2d nominal

data Point2d (coordinateSystem :: CoordinateSystem) where
  Point2d :: Qty units -> Qty units -> Point2d (space @ units)

deriving instance Eq (Point2d (space @ units))

deriving instance Ord (Point2d (space @ units))

deriving instance Show (Point2d (space @ units))

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (Point2d (space @ units1'))
    (Point2d (space' @ units2'))

instance
  (units ~ units', space ~ space') =>
  Addition
    (Point2d (space @ units))
    (Vector2d (space' @ units'))
    (Point2d (space @ units))
  where
  Point2d px py + Vector2d vx vy = Point2d (px + vx) (py + vy)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Point2d (space @ units))
    (Vector2d (space' @ units'))
    (Point2d (space @ units))
  where
  Point2d px py - Vector2d vx vy = Point2d (px - vx) (py - vy)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Point2d (space @ units))
    (Point2d (space' @ units'))
    (Vector2d (space @ units))
  where
  Point2d x1 y1 - Point2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  (units ~ units', space ~ space') =>
  Addition
    (Point2d (space @ units))
    (VectorBounds2d (space' @ units'))
    (Bounds2d (space @ units))
  where
  Point2d px py + VectorBounds2d vx vy = Bounds2d (px + vx) (py + vy)

instance
  (units ~ units', space ~ space') =>
  Subtraction
    (Point2d (space @ units))
    (VectorBounds2d (space' @ units'))
    (Bounds2d (space @ units))
  where
  Point2d px py - VectorBounds2d vx vy = Bounds2d (px - vx) (py - vy)

instance
  (space ~ space', units ~ units') =>
  ApproximateEquality (Point2d (space @ units)) (Point2d (space' @ units')) units
  where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

instance Bounded.Interface (Point2d (space @ units)) (Bounds2d (space @ units)) where
  boundsImpl = Bounds2d.constant

origin :: Point2d (space @ units)
origin = Point2d Qty.zero Qty.zero

x :: Qty units -> Point2d (space @ units)
x px = Point2d px Qty.zero

y :: Qty units -> Point2d (space @ units)
y py = Point2d Qty.zero py

xy :: Qty units -> Qty units -> Point2d (space @ units)
xy = Point2d

apply :: (Float -> Qty units) -> Float -> Float -> Point2d (space @ units)
apply units px py = Point2d (units px) (units py)

meters :: Float -> Float -> Point2d (space @ Meters)
meters = apply Length.meters

centimeters :: Float -> Float -> Point2d (space @ Meters)
centimeters = apply Length.centimeters

millimeters :: Float -> Float -> Point2d (space @ Meters)
millimeters = apply Length.millimeters

inches :: Float -> Float -> Point2d (space @ Meters)
inches = apply Length.inches

xCoordinate :: Point2d (space @ units) -> Qty units
xCoordinate (Point2d px _) = px

yCoordinate :: Point2d (space @ units) -> Qty units
yCoordinate (Point2d _ py) = py

interpolateFrom ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Float ->
  Point2d (space @ units)
interpolateFrom (Point2d x1 y1) (Point2d x2 y2) t =
  Point2d
    (Qty.interpolateFrom x1 x2 t)
    (Qty.interpolateFrom y1 y2 t)

midpoint :: Point2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
midpoint (Point2d x1 y1) (Point2d x2 y2) =
  Point2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

distanceFrom :: Point2d (space @ units) -> Point2d (space @ units) -> Qty units
distanceFrom p1 p2 = Vector2d.magnitude (p2 - p1)

angleFrom :: Point2d (space @ units) -> Point2d (space @ units) -> Angle
angleFrom p1 p2 = Vector2d.angle (p2 - p1)

signedDistanceAlong :: Axis2d (space @ units) -> Point2d (space @ units) -> Qty units
signedDistanceAlong axis point =
  Axis2d.direction axis <> (point - Axis2d.originPoint axis)

signedDistanceFrom :: Axis2d (space @ units) -> Point2d (space @ units) -> Qty units
signedDistanceFrom axis point =
  Axis2d.direction axis >< (point - Axis2d.originPoint axis)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (local @ units) ->
  Point2d (global @ units)
placeIn frame (Point2d px py) =
  let (Point2d x0 y0) = Frame2d.originPoint frame
      (Direction2d (Vector2d ix iy)) = Frame2d.xDirection frame
      (Direction2d (Vector2d jx jy)) = Frame2d.yDirection frame
   in Point2d (x0 + px * ix + py * jx) (y0 + px * iy + py * jy)

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (global @ units) ->
  Point2d (local @ units)
relativeTo frame (Point2d px py) =
  let (Point2d x0 y0) = Frame2d.originPoint frame
      (Direction2d (Vector2d ix iy)) = Frame2d.xDirection frame
      (Direction2d (Vector2d jx jy)) = Frame2d.yDirection frame
      dx = px - x0
      dy = py - y0
   in Point2d (dx * ix + dy * iy) (dx * jx + dy * jy)
