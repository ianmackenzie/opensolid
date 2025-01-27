module OpenSolid.Point2d
  ( Point2d (Point2d#, Point2d)
  , origin
  , x
  , y
  , along
  , xy
  , xyIn
  , fromCoordinates
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

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Arithmetic.Unboxed
import {-# SOURCE #-} OpenSolid.Axis2d (Axis2d)
import {-# SOURCE #-} OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import {-# SOURCE #-} OpenSolid.Bounds2d (Bounds2d)
import {-# SOURCE #-} OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import {-# SOURCE #-} OpenSolid.Frame2d (Frame2d)
import {-# SOURCE #-} OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Length qualified as Length
import OpenSolid.Point2d.CoordinateTransformation qualified as Point2d.CoordinateTransformation
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty#))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (Meters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d#))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Point2d phantom

data Point2d (coordinateSystem :: CoordinateSystem) = Point2d# Double# Double#

{-# COMPLETE Point2d #-}

{-# INLINE Point2d #-}
pattern Point2d :: Qty units -> Qty units -> Point2d (space @ units)
pattern Point2d px py <- (coordinates# -> (# px, py #)) where Point2d = xy

deriving instance Eq (Point2d (space @ units))

deriving instance Ord (Point2d (space @ units))

deriving instance Show (Point2d (space @ units))

instance FFI (Point2d (space @ Meters)) where
  representation = FFI.classRepresentation "Point2d"

instance FFI (Point2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvPoint"

instance HasUnits (Point2d (space @ units)) units (Point2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Point2d (space1 @ unitsA)) (Point2d (space2 @ unitsB))
  where
  coerce = Data.Coerce.coerce

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  Point2d# px# py# + Vector2d# vx# vy# = Point2d# (px# +# vx#) (py# +# vy#)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Point2d (space1 @ units1))
  where
  Point2d# px# py# - Vector2d# vx# vy# = Point2d# (px# -# vx#) (py# -# vy#)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (Vector2d (space1 @ units1))
  where
  Point2d# x1# y1# - Point2d# x2# y2# = Vector2d# (x1# -# x2#) (y1# -# y2#)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Point2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Point2d# px# py# + VectorBounds2d vx vy = Bounds2d.xy (Qty# px# + vx) (Qty# py# + vy)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Point2d# px# py# - VectorBounds2d vx vy = Bounds2d.xy (Qty# px# - vx) (Qty# py# - vy)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Point2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

instance Bounded (Point2d (space @ units)) (Bounds2d (space @ units)) where
  bounds = Bounds2d.constant

-- | The point with coordinates (0,0).
origin :: Point2d (space @ units)
origin = Point2d# 0.0## 0.0##

-- | Construct a point along the X axis, with the given X coordinate.
x :: forall space units. Qty units -> Point2d (space @ units)
x (Qty# px#) = Point2d# px# 0.0##

-- | Construct a point along the Y axis, with the given Y coordinate.
y :: forall space units. Qty units -> Point2d (space @ units)
y (Qty# py#) = Point2d# 0.0## py#

along :: Axis2d (space @ units) -> Qty units -> Point2d (space @ units)
along axis distance = Axis2d.originPoint axis + distance * Axis2d.direction axis

-- | Construct a point from its X and Y coordinates.
xy :: forall space units. Qty units -> Qty units -> Point2d (space @ units)
xy (Qty# px#) (Qty# py#) = Point2d# px# py#

xyIn :: Frame2d (space @ units) defines -> Qty units -> Qty units -> Point2d (space @ units)
xyIn frame px py =
  Frame2d.originPoint frame + px * Frame2d.xDirection frame + py * Frame2d.yDirection frame

-- | Construct a point from a pair of X and Y coordinates.
fromCoordinates :: forall space units. (Qty units, Qty units) -> Point2d (space @ units)
fromCoordinates (px, py) = Point2d px py

apply :: (Float -> Qty units) -> Float -> Float -> Point2d (space @ units)
apply units fx fy = do
  let !(Qty# px#) = units fx
  let !(Qty# py#) = units fy
  Point2d# px# py#

-- | Construct a point from its X and Y coordinates given in meters.
meters :: Float -> Float -> Point2d (space @ Meters)
meters = apply Length.meters

-- | Construct a point from its X and Y coordinates given in centimeters.
centimeters :: Float -> Float -> Point2d (space @ Meters)
centimeters = apply Length.centimeters

-- | Construct a point from its X and Y coordinates given in millimeters.
millimeters :: Float -> Float -> Point2d (space @ Meters)
millimeters = apply Length.millimeters

-- | Construct a point from its X and Y coordinates given in inches.
inches :: Float -> Float -> Point2d (space @ Meters)
inches = apply Length.inches

-- | Get the X coordinate of a point.
xCoordinate :: Point2d (space @ units) -> Qty units
xCoordinate (Point2d# px# _) = Qty# px#

-- | Get the Y coordinate of a point.
yCoordinate :: Point2d (space @ units) -> Qty units
yCoordinate (Point2d# _ py#) = Qty# py#

-- | Get the X and Y coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: Point2d (space @ units) -> (Qty units, Qty units)
coordinates (Point2d# px# py#) = (Qty# px#, Qty# py#)

{-# INLINE coordinates# #-}
coordinates# :: Point2d (space @ units) -> (# Qty units, Qty units #)
coordinates# (Point2d# px# py#) = (# Qty# px#, Qty# py# #)

interpolateFrom ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Float ->
  Point2d (space @ units)
interpolateFrom (Point2d# x1# y1#) (Point2d# x2# y2#) (Qty# t#) =
  Point2d# (x1# +# t# *# (x2# -# x1#)) (y1# +# t# *# (y2# -# y1#))

-- | Find the midpoint between two points.
midpoint :: Point2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
midpoint (Point2d# x1# y1#) (Point2d# x2# y2#) =
  Point2d# (0.5## *# (x1# +# x2#)) (0.5## *# (y1# +# y2#))

-- | Compute the distance from one point to another.
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
convert (Qty# factor#) (Point2d# px# py#) = Point2d# (factor# *# px#) (factor# *# py#)

unconvert :: Qty (units2 :/: units1) -> Point2d (space @ units2) -> Point2d (space @ units1)
unconvert (Qty# factor#) (Point2d# px# py#) = Point2d# (px# /# factor#) (py# /# factor#)

transformBy :: Transform2d tag (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
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
