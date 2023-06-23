module Vector2d
  ( Vector2d (Vector2d, xComponent, yComponent)
  , zero
  , x
  , y
  , xy
  , from
  , meters
  , squareMeters
  , polar
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , angle
  , IsZero (..)
  , direction
  , magnitudeAndDirection
  , normalize
  , placeIn
  , relativeTo
  )
where

import Angle (Angle)
import Angle qualified
import Area qualified
import CoordinateSystem (Units)
import Direction2d (Direction2d (Direction2d))
import Direction2d qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import Generic qualified
import Length qualified
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Qty qualified
import Result qualified
import Units (Meters, SquareMeters, Unitless)
import Units qualified

type role Vector2d phantom

data Vector2d (coordinateSystem :: CoordinateSystem) = Vector2d
  { xComponent :: Qty (Units coordinateSystem)
  , yComponent :: Qty (Units coordinateSystem)
  }
  deriving (Eq, Show)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (Vector2d (space @ units1'))
      (Vector2d (space' @ units2'))

instance Generic.Zero (Vector2d (space @ units)) where
  zero = zero

instance
  (space ~ space', units ~ units')
  => ApproximateEquality (Vector2d (space @ units)) (Vector2d (space' @ units')) units
  where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector2d (space @ units)) where
  negate (Vector2d vx vy) = Vector2d (negate vx) (negate vy)

instance
  (space ~ space', units ~ units')
  => Addition
      (Vector2d (space @ units))
      (Vector2d (space' @ units'))
      (Vector2d (space @ units))
  where
  Vector2d x1 y1 + Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units')
  => Subtraction
      (Vector2d (space @ units))
      (Vector2d (space' @ units'))
      (Vector2d (space @ units))
  where
  Vector2d x1 y1 - Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance
  Units.Product units1 units2 units3
  => Multiplication
      (Qty units1)
      (Vector2d (space @ units2))
      (Vector2d (space @ units3))
  where
  scale * Vector2d vx vy = Vector2d (scale * vx) (scale * vy)

instance
  Units.Product units1 units2 units3
  => Multiplication
      (Vector2d (space @ units1))
      (Qty units2)
      (Vector2d (space @ units3))
  where
  Vector2d vx vy * scale = Vector2d (vx * scale) (vy * scale)

instance
  Units.Quotient units1 units2 units3
  => Division
      (Vector2d (space @ units1))
      (Qty units2)
      (Vector2d (space @ units3))
  where
  Vector2d vx vy / scale = Vector2d (vx / scale) (vy / scale)

instance
  (Units.Product units1 units2 units3, space ~ space')
  => DotProduct
      (Vector2d (space @ units1))
      (Vector2d (space' @ units2))
      (Qty units3)
  where
  Vector2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space ~ space'
  => DotProduct
      (Vector2d (space @ units))
      (Direction2d space')
      (Qty units)
  where
  Vector2d vx vy <> Direction2d dx dy = vx * dx + vy * dy

instance
  space ~ space'
  => DotProduct
      (Direction2d space)
      (Vector2d (space' @ units))
      (Qty units)
  where
  Direction2d dx dy <> Vector2d vx vy = dx * vx + dy * vy

instance
  (Units.Product units1 units2 units3, space ~ space')
  => CrossProduct
      (Vector2d (space @ units1))
      (Vector2d (space' @ units2))
      (Qty units3)
  where
  Vector2d x1 y1 >< Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  space ~ space'
  => CrossProduct
      (Vector2d (space @ units))
      (Direction2d space')
      (Qty units)
  where
  Vector2d vx vy >< Direction2d dx dy = vx * dy - vy * dx

instance
  space ~ space'
  => CrossProduct
      (Direction2d space)
      (Vector2d (space' @ units))
      (Qty units)
  where
  Direction2d dx dy >< Vector2d vx vy = dx * vy - dy * vx

zero :: Vector2d (space @ units)
zero = Vector2d Qty.zero Qty.zero

x :: Qty units -> Vector2d (space @ units)
x vx = Vector2d vx Qty.zero

y :: Qty units -> Vector2d (space @ units)
y vy = Vector2d Qty.zero vy

xy :: Qty units -> Qty units -> Vector2d (space @ units)
xy = Vector2d

from :: Point2d (space @ units) -> Point2d (space @ units) -> Vector2d (space @ units)
from p1 p2 = p2 - p1

meters :: Float -> Float -> Vector2d (space @ Meters)
meters vx vy = Vector2d (Length.meters vx) (Length.meters vy)

squareMeters :: Float -> Float -> Vector2d (space @ SquareMeters)
squareMeters vx vy = Vector2d (Area.squareMeters vx) (Area.squareMeters vy)

polar :: Qty units -> Angle -> Vector2d (space @ units)
polar r theta = Vector2d (r * Angle.cos theta) (r * Angle.sin theta)

interpolateFrom
  :: Vector2d (space @ units)
  -> Vector2d (space @ units)
  -> Float
  -> Vector2d (space @ units)
interpolateFrom (Vector2d x1 y1) (Vector2d x2 y2) t =
  Vector2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units)
midpoint (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

magnitude :: Vector2d (space @ units) -> Qty units
magnitude (Vector2d vx vy) = Qty.hypot2 vx vy

squaredMagnitude :: Units.Squared units1 units2 => Vector2d (space @ units1) -> Qty units2
squaredMagnitude (Vector2d vx vy) = Qty.squared vx + Qty.squared vy

angle :: Vector2d (space @ units) -> Angle
angle (Vector2d vx vy) = Angle.atan2 vy vx

data IsZero = IsZero deriving (Show, ErrorMessage)

direction :: Vector2d (space @ units) -> Result IsZero (Direction2d space)
direction vector = do
  let Vector2d vx vy = vector
  vm <- Qty.nonZero (magnitude vector) |> Result.mapError (\Qty.IsZero -> Vector2d.IsZero)
  Ok (Direction2d.unsafe (vx / vm) (vy / vm))

magnitudeAndDirection :: Vector2d (space @ units) -> Result IsZero (Qty units, Direction2d space)
magnitudeAndDirection vector = do
  let Vector2d vx vy = vector
  vm <- Qty.nonZero (magnitude vector) |> Result.mapError (\Qty.IsZero -> Vector2d.IsZero)
  Ok (vm, Direction2d.unsafe (vx / vm) (vy / vm))

normalize :: Vector2d (space @ units) -> Vector2d (space @ Unitless)
normalize vector =
  let Vector2d vx vy = vector; vm = magnitude vector
   in if vm == Qty.zero then zero else Vector2d (vx / vm) (vy / vm)

placeIn
  :: Frame2d (global @ units) (Defines (local @ units))
  -> Vector2d (local @ units)
  -> Vector2d (global @ units)
placeIn frame (Vector2d vx vy) =
  let (Direction2d ix iy) = Frame2d.xDirection frame
      (Direction2d jx jy) = Frame2d.yDirection frame
   in Vector2d (vx * ix + vy * jx) (vx * iy + vy * jy)

relativeTo
  :: Frame2d (global @ units) (Defines (local @ units))
  -> Vector2d (global @ units)
  -> Vector2d (local @ units)
relativeTo frame (Vector2d vx vy) =
  let (Direction2d ix iy) = Frame2d.xDirection frame
      (Direction2d jx jy) = Frame2d.yDirection frame
   in Vector2d (vx * ix + vy * iy) (vx * jx + vy * jy)
