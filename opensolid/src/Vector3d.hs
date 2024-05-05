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
  , centimeters
  , millimeters
  , inches
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
import Data.Coerce qualified
import {-# SOURCE #-} Direction3d (Direction3d)
import {-# SOURCE #-} Direction3d qualified
import Length qualified
import OpenSolid
import Qty qualified
import Units (Meters, SquareMeters)
import Units qualified

type role Vector3d phantom

data Vector3d (coordinateSystem :: CoordinateSystem) where
  Vector3d ::
    Qty (Units coordinateSystem) ->
    Qty (Units coordinateSystem) ->
    Qty (Units coordinateSystem) ->
    Vector3d coordinateSystem

deriving instance Eq (Vector3d (space @ units))

deriving instance Show (Vector3d (space @ units))

instance HasUnits (Vector3d (space @ units)) where
  type Units (Vector3d (space @ units)) = units
  type Erase (Vector3d (space @ units)) = Vector3d (space @ Unitless)

instance space ~ space_ => Units.Coercion (Vector3d (space @ units1)) (Vector3d (space_ @ units2)) where
  coerce = Data.Coerce.coerce

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  ApproximateEquality (Vector3d (space @ units)) (Vector3d (space_ @ units_)) units
  where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector3d (space @ units)) where
  negate (Vector3d vx vy vz) = Vector3d (negate vx) (negate vy) (negate vz)

instance Multiplication Sign (Vector3d (space @ units)) where
  type Sign .*. Vector3d (space @ units) = Vector3d (space @ (Unitless :*: units))
  Positive .*. vector = Units.coerce vector
  Negative .*. vector = Units.coerce -vector

instance Product Sign (Vector3d (space @ units)) (Vector3d (space @ units))

instance Multiplication (Vector3d (space @ units)) Sign where
  type Vector3d (space @ units) .*. Sign = Vector3d (space @ (units :*: Unitless))
  vector .*. Positive = Units.coerce vector
  vector .*. Negative = Units.coerce -vector

instance Product (Vector3d (space @ units)) Sign (Vector3d (space @ units))

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (Vector3d (space_ @ units_))
    (Vector3d (space @ units))
  where
  Vector3d x1 y1 z1 + Vector3d x2 y2 z2 = Vector3d (x1 + x2) (y1 + y2) (z1 + z2)

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (Vector3d (space_ @ units_))
    (Vector3d (space @ units))
  where
  Vector3d x1 y1 z1 - Vector3d x2 y2 z2 = Vector3d (x1 - x2) (y1 - y2) (z1 - z2)

instance Multiplication (Qty units1) (Vector3d (space @ units2)) where
  type Qty units1 .*. Vector3d (space @ units2) = Vector3d (space @ (units1 :*: units2))
  scale .*. Vector3d vx vy vz = Vector3d (scale .*. vx) (scale .*. vy) (scale .*. vz)

instance
  Units.Product units1 units2 units3 =>
  Product (Qty units1) (Vector3d (space @ units2)) (Vector3d (space @ units3))

instance Multiplication (Vector3d (space @ units1)) (Qty units2) where
  type Vector3d (space @ units1) .*. Qty units2 = Vector3d (space @ (units1 :*: units2))
  Vector3d vx vy vz .*. scale = Vector3d (vx .*. scale) (vy .*. scale) (vz .*. scale)

instance
  Units.Product units1 units2 units3 =>
  Product (Vector3d (space @ units1)) (Qty units2) (Vector3d (space @ units3))

instance Division (Vector3d (space @ units1)) (Qty units2) where
  type Vector3d (space @ units1) ./. Qty units2 = Vector3d (space @ (units1 :/: units2))
  Vector3d vx vy vz ./. scale = Vector3d (vx ./. scale) (vy ./. scale) (vz ./. scale)

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (Vector3d (space @ units1)) (Qty units2) (Vector3d (space @ units3))

instance
  space ~ space_ =>
  DotMultiplication (Vector3d (space @ units1)) (Vector3d (space_ @ units2))
  where
  type Vector3d (space @ units1) .<>. Vector3d (space_ @ units2) = Qty (units1 :*: units2)
  Vector3d x1 y1 z1 .<>. Vector3d x2 y2 z2 = x1 .*. x2 + y1 .*. y2 + z1 .*. z2

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  DotProduct (Vector3d (space @ units1)) (Vector3d (space_ @ units2)) (Qty units3)

instance
  space ~ space_ =>
  CrossMultiplication (Vector3d (space @ units1)) (Vector3d (space_ @ units2))
  where
  type Vector3d (space @ units1) .><. Vector3d (space_ @ units2) = Vector3d (space @ (units1 :*: units2))
  Vector3d x1 y1 z1 .><. Vector3d x2 y2 z2 =
    Vector3d
      (y1 .*. z2 - z1 .*. y2)
      (z1 .*. x2 - x1 .*. z2)
      (x1 .*. y2 - y1 .*. x2)

instance
  (Units.Product units1 units2 units3, space ~ space_) =>
  CrossProduct (Vector3d (space @ units1)) (Vector3d (space_ @ units2)) (Vector3d (space @ units3))

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

apply :: (Float -> Qty units) -> Float -> Float -> Float -> Vector3d (space @ units)
apply units px py pz = Vector3d (units px) (units py) (units pz)

meters :: Float -> Float -> Float -> Vector3d (space @ Meters)
meters = apply Length.meters

centimeters :: Float -> Float -> Float -> Vector3d (space @ Meters)
centimeters = apply Length.centimeters

millimeters :: Float -> Float -> Float -> Vector3d (space @ Meters)
millimeters = apply Length.millimeters

inches :: Float -> Float -> Float -> Vector3d (space @ Meters)
inches = apply Length.inches

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

data IsZero = IsZero deriving (Eq, Show, Error)

direction :: Tolerance units => Vector3d (space @ units) -> Result IsZero (Direction3d space)
direction vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Error Vector3d.IsZero else Ok (Direction3d.unsafe (vector / vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector3d (space @ units) ->
  Result IsZero (Qty units, Direction3d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Error Vector3d.IsZero else Ok (vm, Direction3d.unsafe (vector / vm))

normalize :: Vector3d (space @ units) -> Vector3d (space @ Unitless)
normalize vector = do
  let vm = magnitude vector
  if vm == Qty.zero then zero else vector / vm
