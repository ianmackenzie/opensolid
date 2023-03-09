module Vector2d
  ( Vector2d (..)
  , zero
  , x
  , y
  , xy
  , meters
  , squareMeters
  , polar
  , midpoint
  , interpolateFrom
  , determinant
  , magnitude
  , squaredMagnitude
  , angle
  , IsZero (..)
  , direction
  , magnitudeAndDirection
  , normalize
  )
where

import Angle (Angle)
import Angle qualified
import Area qualified
import {-# SOURCE #-} Direction2d (Direction2d)
import {-# SOURCE #-} Direction2d qualified
import Generic qualified
import Length qualified
import OpenSolid
import Qty qualified
import Result qualified
import Units (Meters, SquareMeters, Unitless)
import Units qualified

type role Vector2d nominal nominal

type Vector2d :: Type -> Type -> Type
data Vector2d coordinates units = Vector2d (Qty units) (Qty units)
  deriving (Eq, Show)

instance Units.Coercion (Vector2d coordinates)

instance Generic.Zero (Vector2d coordinates units) where
  zero = zero

instance
  (coordinates ~ coordinates', units ~ units')
  => ApproximateEquality (Vector2d coordinates units) (Vector2d coordinates' units') units
  where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector2d coordinates units) where
  negate (Vector2d vx vy) = Vector2d (negate vx) (negate vy)

instance (units ~ units', coordinates ~ coordinates') => Addition (Vector2d coordinates units) (Vector2d coordinates' units') (Vector2d coordinates units) where
  Vector2d x1 y1 + Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Vector2d coordinates units) (Vector2d coordinates' units') (Vector2d coordinates units) where
  Vector2d x1 y1 - Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (Vector2d coordinates units2) (Vector2d coordinates units3) where
  scale * Vector2d vx vy = Vector2d (scale * vx) (scale * vy)

instance Units.Product units1 units2 units3 => Multiplication (Vector2d coordinates units1) (Qty units2) (Vector2d coordinates units3) where
  Vector2d vx vy * scale = Vector2d (vx * scale) (vy * scale)

instance Units.Quotient units1 units2 units3 => Division (Vector2d coordinates units1) (Qty units2) (Vector2d coordinates units3) where
  Vector2d vx vy / scale = Vector2d (vx / scale) (vy / scale)

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (Vector2d coordinates units1) (Vector2d coordinates' units2) (Qty units3) where
  Vector2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => CrossProduct (Vector2d coordinates units1) (Vector2d coordinates' units2) (Qty units3) where
  Vector2d x1 y1 >< Vector2d x2 y2 = x1 * y2 - y1 * x2

zero :: Vector2d coordinates units
zero = Vector2d Qty.zero Qty.zero

x :: Qty units -> Vector2d coordinates units
x vx = Vector2d vx Qty.zero

y :: Qty units -> Vector2d coordinates units
y vy = Vector2d Qty.zero vy

xy :: Qty units -> Qty units -> Vector2d coordinates units
xy = Vector2d

meters :: Float -> Float -> Vector2d coordinates Meters
meters vx vy = Vector2d (Length.meters vx) (Length.meters vy)

squareMeters :: Float -> Float -> Vector2d coordinates SquareMeters
squareMeters vx vy = Vector2d (Area.squareMeters vx) (Area.squareMeters vy)

polar :: Qty units -> Angle -> Vector2d coordinates units
polar r theta = Vector2d (r * Angle.cos theta) (r * Angle.sin theta)

interpolateFrom
  :: Vector2d coordinates units
  -> Vector2d coordinates units
  -> Float
  -> Vector2d coordinates units
interpolateFrom (Vector2d x1 y1) (Vector2d x2 y2) t =
  Vector2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Vector2d coordinates units -> Vector2d coordinates units -> Vector2d coordinates units
midpoint (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

determinant
  :: Units.Product units1 units2 units3
  => Vector2d coordinates units1
  -> Vector2d coordinates units2
  -> Qty units3
determinant (Vector2d x1 y1) (Vector2d x2 y2) = x1 * y2 - y1 * x2

magnitude :: Vector2d coordinates units -> Qty units
magnitude (Vector2d vx vy) = Qty.hypot2 vx vy

squaredMagnitude :: Units.Squared units1 units2 => Vector2d coordinates units1 -> Qty units2
squaredMagnitude (Vector2d vx vy) = Qty.squared vx + Qty.squared vy

angle :: Vector2d coordinates units -> Angle
angle (Vector2d vx vy) = Angle.atan2 vy vx

data IsZero = IsZero

instance IsError IsZero where
  errorMessage IsZero = "Vector2d is zero"

direction :: Vector2d coordinates units -> Result IsZero (Direction2d coordinates)
direction vector = Result.do
  let Vector2d vx vy = vector
  vm <- validate (/= Qty.zero) (magnitude vector) ?? Error IsZero
  Ok (Direction2d.unsafe (vx / vm) (vy / vm))

magnitudeAndDirection :: Vector2d coordinates units -> Result IsZero (Qty units, Direction2d coordinates)
magnitudeAndDirection vector = Result.do
  let Vector2d vx vy = vector
  vm <- validate (/= Qty.zero) (magnitude vector) ?? Error IsZero
  Ok (vm, Direction2d.unsafe (vx / vm) (vy / vm))

normalize :: Vector2d coordinates units -> Vector2d coordinates Unitless
normalize vector =
  let Vector2d vx vy = vector; vm = magnitude vector
   in if vm == Qty.zero then zero else Vector2d (vx / vm) (vy / vm)
