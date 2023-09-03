module Vector3d
  ( Vector3d (Vector3d)
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
  , xComponent
  , yComponent
  , zComponent
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , IsZero (IsZero)
  , direction
  , magnitudeAndDirection
  , normalize
  )
where

import Area qualified
import CoordinateSystem (Units)
import {-# SOURCE #-} Direction3d (Direction3d)
import {-# SOURCE #-} Direction3d qualified
import Generic qualified
import Length qualified
import OpenSolid
import Qty qualified
import Units (Meters, SquareMeters)
import Units qualified

data Vector3d (coordinateSystem :: CoordinateSystem)
  = Vector3d
      (Qty (Units coordinateSystem))
      (Qty (Units coordinateSystem))
      (Qty (Units coordinateSystem))
  deriving (Eq, Show)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (Vector3d (space @ units1'))
    (Vector3d (space' @ units2'))

instance Generic.Zero (Vector3d (space @ units)) where
  zero = zero

instance
  (space ~ space', units ~ units') =>
  ApproximateEquality (Vector3d (space @ units)) (Vector3d (space' @ units')) units
  where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector3d (space @ units)) where
  negate (Vector3d vx vy vz) = Vector3d (negate vx) (negate vy) (negate vz)

instance
  Multiplication
    Sign
    (Vector3d (space @ units))
    (Vector3d (space @ units))
  where
  Positive * vector = vector
  Negative * vector = -vector

instance
  Multiplication
    (Vector3d (space @ units))
    Sign
    (Vector3d (space @ units))
  where
  vector * Positive = vector
  vector * Negative = -vector

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Addition
    (Vector3d (space @ units))
    (Vector3d (space' @ units'))
    (Vector3d (space @ units))
  where
  Vector3d x1 y1 z1 + Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space ~ space'
  , units ~ units'
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Vector3d (space' @ units'))
    (Vector3d (space @ units))
  where
  Vector3d x1 y1 z1 - Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (Vector3d (space @ units2))
    (Vector3d (space @ units3))
  where
  scale * Vector3d vx vy vz = Vector3d (scale * vx) (scale * vy) (scale * vz)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector3d (space @ units1))
    (Qty units2)
    (Vector3d (space @ units3))
  where
  Vector3d vx vy vz * scale = Vector3d (vx * scale) (vy * scale) (vz * scale)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Vector3d (space @ units1))
    (Qty units2)
    (Vector3d (space @ units3))
  where
  Vector3d vx vy vz / scale = Vector3d (vx / scale) (vy / scale) (vz / scale)

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  DotProduct
    (Vector3d (space @ units1))
    (Vector3d (space' @ units2))
    (Qty units3)
  where
  Vector3d x1 y1 z1 <> Vector3d x2 y2 z2 = x1 * x2 + y1 * y2 + z1 * z2

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  CrossProduct
    (Vector3d (space @ units1))
    (Vector3d (space' @ units2))
    (Vector3d (space @ units3))
  where
  Vector3d x1 y1 z1 >< Vector3d x2 y2 z2 =
    Vector3d
      (y1 * z2 - z1 * y2)
      (z1 * x2 - x1 * z2)
      (x1 * y2 - y1 * x2)

zero :: Vector3d (space @ units)
zero = Vector3d Qty.zero Qty.zero Qty.zero

x :: Qty units -> Vector3d (space @ units)
x vx = Vector3d vx Qty.zero Qty.zero

y :: Qty units -> Vector3d (space @ units)
y vy = Vector3d Qty.zero vy Qty.zero

z :: Qty units -> Vector3d (space @ units)
z vz = Vector3d Qty.zero Qty.zero vz

xy :: Qty units -> Qty units -> Vector3d (space @ units)
xy vx vz = Vector3d vx vz Qty.zero

xz :: Qty units -> Qty units -> Vector3d (space @ units)
xz vx vz = Vector3d vx Qty.zero vz

yz :: Qty units -> Qty units -> Vector3d (space @ units)
yz vy vz = Vector3d Qty.zero vy vz

xyz :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)
xyz = Vector3d

meters :: Float -> Float -> Float -> Vector3d (space @ Meters)
meters vx vy vz = Vector3d (Length.meters vx) (Length.meters vy) (Length.meters vz)

squareMeters :: Float -> Float -> Float -> Vector3d (space @ SquareMeters)
squareMeters vx vy vz =
  Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

xComponent :: Vector3d (space @ units) -> Qty units
xComponent (Vector3d vx _ _) = vx

yComponent :: Vector3d (space @ units) -> Qty units
yComponent (Vector3d _ vy _) = vy

zComponent :: Vector3d (space @ units) -> Qty units
zComponent (Vector3d _ _ vz) = vz

interpolateFrom ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Float ->
  Vector3d (space @ units)
interpolateFrom (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) t =
  Vector3d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t) (Qty.interpolateFrom z1 z2 t)

midpoint :: Vector3d (space @ units) -> Vector3d (space @ units) -> Vector3d (space @ units)
midpoint (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  Vector3d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2) (Qty.midpoint z1 z2)

magnitude :: Vector3d (space @ units) -> Qty units
magnitude (Vector3d vx vy vz) = Qty.hypot3 vx vy vz

squaredMagnitude :: Units.Squared units1 units2 => Vector3d (space @ units1) -> Qty units2
squaredMagnitude (Vector3d vx vy vz) = Qty.squared vx + Qty.squared vy + Qty.squared vz

data IsZero = IsZero deriving (Eq, Show, ErrorMessage)

direction :: Tolerance units => Vector3d (space @ units) -> Result IsZero (Direction3d space)
direction vector = do
  let vm = magnitude vector
   in if vm ~= Qty.zero then Error Vector3d.IsZero else Ok (Direction3d.unsafe (vector / vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector3d (space @ units) ->
  Result IsZero (Qty units, Direction3d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
   in if vm ~= Qty.zero then Error Vector3d.IsZero else Ok (vm, Direction3d.unsafe (vector / vm))

normalize :: Vector3d (space @ units) -> Vector3d (space @ Unitless)
normalize vector =
  let vm = magnitude vector
   in if vm == Qty.zero then zero else vector / vm
