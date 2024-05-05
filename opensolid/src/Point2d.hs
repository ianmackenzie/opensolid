module Point2d
  ( Point2d
  , origin
  , x
  , y
  , along
  , xy
  , xyIn
  , meters
  , centimeters
  , millimeters
  , inches
  , xCoordinate
  , yCoordinate
  , coordinates
  , midpoint
  , interpolateFrom
  , distanceFrom
  , angleFrom
  , signedDistanceAlong
  , signedDistanceFrom
  , placeIn
  , relativeTo
  , convert
  , unconvert
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  )
where

import {-# SOURCE #-} Axis2d (Axis2d)
import {-# SOURCE #-} Axis2d qualified
import Bounded qualified
import {-# SOURCE #-} Bounds2d (Bounds2d)
import {-# SOURCE #-} Bounds2d qualified
import Data.Coerce qualified
import Direction2d (Direction2d)
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import Length qualified
import OpenSolid
import Point2d.CoordinateTransformation qualified
import Qty qualified
import Transform2d (Transform2d (Transform2d))
import Transform2d qualified
import Units (Meters)
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Point2d phantom

data Point2d (coordinateSystem :: CoordinateSystem) where
  Point2d ::
    Qty (Units coordinateSystem) ->
    Qty (Units coordinateSystem) ->
    Point2d coordinateSystem

deriving instance Eq (Point2d (space @ units))

deriving instance Ord (Point2d (space @ units))

deriving instance Show (Point2d (space @ units))

instance HasUnits (Point2d (space @ units)) where
  type Units (Point2d (space @ units)) = units
  type Erase (Point2d (space @ units)) = Point2d (space @ Unitless)

instance
  space ~ space_ =>
  Units.Coercion (Point2d (space @ units1)) (Point2d (space_ @ units2))
  where
  coerce = Data.Coerce.coerce

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Point2d (space @ units))
    (Vector2d (space_ @ units_))
    (Point2d (space @ units))
  where
  Point2d px py + Vector2d vx vy = Point2d (px + vx) (py + vy)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point2d (space @ units))
    (Vector2d (space_ @ units_))
    (Point2d (space @ units))
  where
  Point2d px py - Vector2d vx vy = Point2d (px - vx) (py - vy)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point2d (space @ units))
    (Point2d (space_ @ units_))
    (Vector2d (space @ units))
  where
  Point2d x1 y1 - Point2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Point2d (space @ units))
    (VectorBounds2d (space_ @ units_))
    (Bounds2d (space @ units))
  where
  Point2d px py + VectorBounds2d vx vy = Bounds2d.xy (px + vx) (py + vy)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Point2d (space @ units))
    (VectorBounds2d (space_ @ units_))
    (Bounds2d (space @ units))
  where
  Point2d px py - VectorBounds2d vx vy = Bounds2d.xy (px - vx) (py - vy)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  ApproximateEquality (Point2d (space @ units)) (Point2d (space_ @ units_)) units
  where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

instance Bounded.Interface (Point2d (space @ units)) (Bounds2d (space @ units)) where
  bounds = Bounds2d.constant

origin :: Point2d (space @ units)
origin = Point2d Qty.zero Qty.zero

x :: Qty units -> Point2d (space @ units)
x px = Point2d px Qty.zero

y :: Qty units -> Point2d (space @ units)
y py = Point2d Qty.zero py

along :: Axis2d (space @ units) -> Qty units -> Point2d (space @ units)
along axis distance = Axis2d.originPoint axis + distance * Axis2d.direction axis

xy :: Qty units -> Qty units -> Point2d (space @ units)
xy = Point2d

xyIn :: Frame2d (space @ units) defines -> Qty units -> Qty units -> Point2d (space @ units)
xyIn frame px py =
  Frame2d.originPoint frame + px * Frame2d.xDirection frame + py * Frame2d.yDirection frame

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

{-# INLINE coordinates #-}
coordinates :: Point2d (space @ units) -> (Qty units, Qty units)
coordinates (Point2d px py) = (px, py)

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
placeIn = Point2d.CoordinateTransformation.placeIn

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (global @ units) ->
  Point2d (local @ units)
relativeTo = Point2d.CoordinateTransformation.relativeTo

convert :: Qty (units2 :/: units1) -> Point2d (space @ units1) -> Point2d (space @ units2)
convert conversion (Point2d px py) = Point2d (Qty.convert conversion px) (Qty.convert conversion py)

unconvert :: Qty (units2 :/: units1) -> Point2d (space @ units2) -> Point2d (space @ units1)
unconvert conversion (Point2d px py) = Point2d (Qty.unconvert conversion px) (Qty.unconvert conversion py)

transformBy :: Transform2d a (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
transformBy transform point = do
  let (Transform2d p0 vx vy) = transform
  let (px, py) = coordinates point
  p0 + px * vx + py * vy

translateBy :: Vector2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
translateBy vector = (+ vector)

translateIn :: Direction2d space -> Qty units -> Point2d (space @ units) -> Point2d (space @ units)
translateIn direction distance = translateBy (direction * distance)

translateAlong :: Axis2d (space @ units) -> Qty units -> Point2d (space @ units) -> Point2d (space @ units)
translateAlong axis distance = translateIn (Axis2d.direction axis) distance

rotateAround :: Point2d (space @ units) -> Angle -> Point2d (space @ units) -> Point2d (space @ units)
rotateAround centerPoint angle = transformBy (Transform2d.rotateAround centerPoint angle)

mirrorAcross :: Axis2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
mirrorAcross axis = transformBy (Transform2d.mirrorAcross axis)

scaleAbout :: Point2d (space @ units) -> Float -> Point2d (space @ units) -> Point2d (space @ units)
scaleAbout centerPoint scale = transformBy (Transform2d.scaleAbout centerPoint scale)

scaleAlong :: Axis2d (space @ units) -> Float -> Point2d (space @ units) -> Point2d (space @ units)
scaleAlong axis scale = transformBy (Transform2d.scaleAlong axis scale)
