module Vector3d
  ( Vector3d (..)
  , zero
  , x
  , y
  , z
  , xy
  , xz
  , yz
  , xyz
  , meters
  , squareMeters
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , IsZero (..)
  , direction
  , magnitudeAndDirection
  , normalize
  )
where

import Area qualified
import {-# SOURCE #-} Direction3d (Direction3d)
import {-# SOURCE #-} Direction3d qualified
import Generic qualified
import Length qualified
import OpenSolid
import Qty qualified
import Units (Meters, SquareMeters, Unitless)
import Units qualified

data Vector3d coordinates units = Vector3d (Qty units) (Qty units) (Qty units)
  deriving (Eq, Show)

instance Units.Coercion (Vector3d coordinates)

instance Generic.Zero (Vector3d coordinates units) where
  zero = zero

instance ApproximateEquality (Vector3d coordinates units) units where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector3d coordinates units) where
  negate (Vector3d vx vy vz) = Vector3d (negate vx) (negate vy) (negate vz)

instance (units ~ units', coordinates ~ coordinates') => Addition (Vector3d coordinates units) (Vector3d coordinates' units') (Vector3d coordinates units) where
  Vector3d x1 y1 z1 + Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Vector3d coordinates units) (Vector3d coordinates' units') (Vector3d coordinates units) where
  Vector3d x1 y1 z1 - Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (Vector3d coordinates units2) (Vector3d coordinates units3) where
  scale * Vector3d vx vy vz = Vector3d (scale * vx) (scale * vy) (scale * vz)

instance Units.Product units1 units2 units3 => Multiplication (Vector3d coordinates units1) (Qty units2) (Vector3d coordinates units3) where
  Vector3d vx vy vz * scale = Vector3d (vx * scale) (vy * scale) (vz * scale)

instance Units.Quotient units1 units2 units3 => Division (Vector3d coordinates units1) (Qty units2) (Vector3d coordinates units3) where
  Vector3d vx vy vz / scale = Vector3d (vx / scale) (vy / scale) (vz / scale)

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (Vector3d coordinates units1) (Vector3d coordinates' units2) (Qty units3) where
  Vector3d x1 y1 z1 <> Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => CrossProduct (Vector3d coordinates units1) (Vector3d coordinates' units2) (Vector3d coordinates units3) where
  Vector3d x1 y1 z1 >< Vector3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

zero :: Vector3d coordinates units
zero = Vector3d Qty.zero Qty.zero Qty.zero

x :: Qty units -> Vector3d coordinates units
x vx = Vector3d vx Qty.zero Qty.zero

y :: Qty units -> Vector3d coordinates units
y vy = Vector3d Qty.zero vy Qty.zero

z :: Qty units -> Vector3d coordinates units
z vz = Vector3d Qty.zero Qty.zero vz

xy :: Qty units -> Qty units -> Vector3d coordinates units
xy vx vz = Vector3d vx vz Qty.zero

xz :: Qty units -> Qty units -> Vector3d coordinates units
xz vx vz = Vector3d vx Qty.zero vz

yz :: Qty units -> Qty units -> Vector3d coordinates units
yz vy vz = Vector3d Qty.zero vy vz

xyz :: Qty units -> Qty units -> Qty units -> Vector3d coordinates units
xyz = Vector3d

meters :: Float -> Float -> Float -> Vector3d coordinates Meters
meters vx vy vz = Vector3d (Length.meters vx) (Length.meters vy) (Length.meters vz)

squareMeters :: Float -> Float -> Float -> Vector3d coordinates SquareMeters
squareMeters vx vy vz =
  Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

interpolateFrom
  :: Vector3d coordinates units
  -> Vector3d coordinates units
  -> Float
  -> Vector3d coordinates units
interpolateFrom (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) t =
  Vector3d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t) (Qty.interpolateFrom z1 z2 t)

midpoint :: Vector3d coordinates units -> Vector3d coordinates units -> Vector3d coordinates units
midpoint (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  Vector3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

magnitude :: Vector3d coordinates units -> Qty units
magnitude (Vector3d vx vy vz) = Qty.hypot3 vx vy vz

squaredMagnitude :: Units.Squared units1 units2 => Vector3d coordinates units1 -> Qty units2
squaredMagnitude (Vector3d vx vy vz) = Qty.squared vx + Qty.squared vy + Qty.squared vz

data IsZero = IsZero

direction :: Vector3d coordinates units -> Result IsZero (Direction3d coordinates)
direction vector@(Vector3d vx vy vz) = do
  magnitude' <- validate (/= Qty.zero) (magnitude vector) ?? Error IsZero
  Ok (Direction3d.unsafe (vx / magnitude') (vy / magnitude') (vz / magnitude'))

magnitudeAndDirection :: Vector3d coordinates units -> Result IsZero (Qty units, Direction3d coordinates)
magnitudeAndDirection vector@(Vector3d vx vy vz) = do
  magnitude' <- validate (/= Qty.zero) (magnitude vector) ?? Error IsZero
  Ok (magnitude', Direction3d.unsafe (vx / magnitude') (vy / magnitude') (vz / magnitude'))

normalize :: Vector3d coordinates units -> Vector3d coordinates Unitless
normalize vector@(Vector3d vx vy vz) =
  let magnitude' = magnitude vector
   in if magnitude' == Qty.zero
        then zero
        else Vector3d (vx / magnitude') (vy / magnitude') (vz / magnitude')
