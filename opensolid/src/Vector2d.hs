module Vector2d
  ( Vector2d (Vector2d)
  , zero
  , x
  , y
  , xy
  , xyIn
  , xyInBasis
  , from
  , meters
  , centimeters
  , millimeters
  , inches
  , squareMeters
  , polar
  , xComponent
  , yComponent
  , components
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , squaredMagnitude_
  , angle
  , IsZero (IsZero)
  , direction
  , magnitudeAndDirection
  , normalize
  , rotateRight
  , rotateLeft
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , sum
  )
where

import Angle qualified
import Area qualified
import {-# SOURCE #-} Basis2d (Basis2d)
import {-# SOURCE #-} Basis2d qualified
import Data.Coerce qualified
import {-# SOURCE #-} Direction2d (Direction2d)
import {-# SOURCE #-} Direction2d qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import Length qualified
import List qualified
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Qty qualified
import Units (Meters, SquareMeters)
import Units qualified

type role Vector2d phantom

data Vector2d (coordinateSystem :: CoordinateSystem) where
  Vector2d ::
    Qty (Units coordinateSystem) ->
    Qty (Units coordinateSystem) ->
    Vector2d coordinateSystem

deriving instance Eq (Vector2d (space @ units))

deriving instance Show (Vector2d (space @ units))

instance HasUnits (Vector2d (space @ units)) where
  type Units (Vector2d (space @ units)) = units
  type Erase (Vector2d (space @ units)) = Vector2d (space @ Unitless)

instance space ~ space' => Units.Coercion (Vector2d (space @ units1)) (Vector2d (space' @ units2)) where
  coerce = Data.Coerce.coerce

instance
  (space ~ space', units ~ units') =>
  ApproximateEquality (Vector2d (space @ units)) (Vector2d (space' @ units')) units
  where
  v1 ~= v2 = magnitude (v1 - v2) ~= Qty.zero

instance Negation (Vector2d (space @ units)) where
  negate (Vector2d vx vy) = Vector2d (negate vx) (negate vy)

instance Multiplication Sign (Vector2d (space @ units)) where
  type Sign .*. Vector2d (space @ units) = Vector2d (space @ (Unitless :*: units))
  Positive .*. vector = Units.coerce vector
  Negative .*. vector = Units.coerce -vector

instance Product Sign (Vector2d (space @ units)) (Vector2d (space @ units))

instance Multiplication (Vector2d (space @ units)) Sign where
  type Vector2d (space @ units) .*. Sign = Vector2d (space @ (units :*: Unitless))
  vector .*. Positive = Units.coerce vector
  vector .*. Negative = Units.coerce -vector

instance Product (Vector2d (space @ units)) Sign (Vector2d (space @ units))

instance
  (space ~ space', units ~ units') =>
  Addition
    (Vector2d (space @ units))
    (Vector2d (space' @ units'))
    (Vector2d (space @ units))
  where
  Vector2d x1 y1 + Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (Vector2d (space @ units))
    (Vector2d (space' @ units'))
    (Vector2d (space @ units))
  where
  Vector2d x1 y1 - Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance Multiplication (Qty units1) (Vector2d (space @ units2)) where
  type Qty units1 .*. Vector2d (space @ units2) = Vector2d (space @ (units1 :*: units2))
  scale .*. Vector2d vx vy = Vector2d (scale .*. vx) (scale .*. vy)

instance
  Units.Product units1 units2 units3 =>
  Product (Qty units1) (Vector2d (space @ units2)) (Vector2d (space @ units3))

instance Multiplication (Vector2d (space @ units1)) (Qty units2) where
  type Vector2d (space @ units1) .*. Qty units2 = Vector2d (space @ (units1 :*: units2))
  Vector2d vx vy .*. scale = Vector2d (vx .*. scale) (vy .*. scale)

instance
  Units.Product units1 units2 units3 =>
  Product (Vector2d (space @ units1)) (Qty units2) (Vector2d (space @ units3))

instance Division (Vector2d (space @ units1)) (Qty units2) where
  type Vector2d (space @ units1) ./. Qty units2 = Vector2d (space @ (units1 :/: units2))
  Vector2d vx vy ./. scale = Vector2d (vx ./. scale) (vy ./. scale)

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (Vector2d (space @ units1)) (Qty units2) (Vector2d (space @ units3))

instance
  space ~ space' =>
  DotMultiplication (Vector2d (space @ units1)) (Vector2d (space' @ units2))
  where
  type Vector2d (space @ units1) .<>. Vector2d (space' @ units2) = Qty (units1 :*: units2)
  Vector2d x1 y1 .<>. Vector2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct (Vector2d (space @ units1)) (Vector2d (space' @ units2)) (Qty units3)

instance space ~ space' => DotMultiplication (Vector2d (space @ units)) (Direction2d space') where
  type Vector2d (space @ units) .<>. Direction2d space' = Qty (units :*: Unitless)
  v .<>. d = v .<>. Direction2d.vector d

instance space ~ space' => DotProduct (Vector2d (space @ units)) (Direction2d space') (Qty units)

instance space ~ space' => DotMultiplication (Direction2d space) (Vector2d (space' @ units)) where
  type Direction2d space .<>. Vector2d (space' @ units) = Qty (Unitless :*: units)
  d .<>. v = Direction2d.vector d .<>. v

instance space ~ space' => DotProduct (Direction2d space) (Vector2d (space' @ units)) (Qty units)

instance
  space ~ space' =>
  CrossMultiplication (Vector2d (space @ units1)) (Vector2d (space' @ units2))
  where
  type Vector2d (space @ units1) .><. Vector2d (space' @ units2) = Qty (units1 :*: units2)
  Vector2d x1 y1 .><. Vector2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct (Vector2d (space @ units1)) (Vector2d (space' @ units2)) (Qty units3)

instance space ~ space' => CrossMultiplication (Vector2d (space @ units)) (Direction2d space') where
  type Vector2d (space @ units) .><. Direction2d space' = Qty (units :*: Unitless)
  v .><. d = v .><. Direction2d.vector d

instance space ~ space' => CrossProduct (Vector2d (space @ units)) (Direction2d space') (Qty units)

instance space ~ space' => CrossMultiplication (Direction2d space) (Vector2d (space' @ units)) where
  type Direction2d space .><. Vector2d (space' @ units) = Qty (Unitless :*: units)
  d .><. v = Direction2d.vector d .><. v

instance space ~ space' => CrossProduct (Direction2d space) (Vector2d (space' @ units)) (Qty units)

zero :: Vector2d (space @ units)
zero = Vector2d Qty.zero Qty.zero

x :: Qty units -> Vector2d (space @ units)
x vx = Vector2d vx Qty.zero

y :: Qty units -> Vector2d (space @ units)
y vy = Vector2d Qty.zero vy

xy :: Qty units -> Qty units -> Vector2d (space @ units)
xy = Vector2d

xyIn :: Frame2d (space @ frameUnits) defines -> Qty units -> Qty units -> Vector2d (space @ units)
xyIn frame = xyInBasis (Frame2d.basis frame)

xyInBasis :: Basis2d space defines -> Qty units -> Qty units -> Vector2d (space @ units)
xyInBasis basis vx vy = vx * Basis2d.xDirection basis + vy * Basis2d.yDirection basis

from :: Point2d (space @ units) -> Point2d (space @ units) -> Vector2d (space @ units)
from p1 p2 = p2 - p1

apply :: (Float -> Qty units) -> Float -> Float -> Vector2d (space @ units)
apply units px py = Vector2d (units px) (units py)

meters :: Float -> Float -> Vector2d (space @ Meters)
meters = apply Length.meters

centimeters :: Float -> Float -> Vector2d (space @ Meters)
centimeters = apply Length.centimeters

millimeters :: Float -> Float -> Vector2d (space @ Meters)
millimeters = apply Length.millimeters

inches :: Float -> Float -> Vector2d (space @ Meters)
inches = apply Length.inches

squareMeters :: Float -> Float -> Vector2d (space @ SquareMeters)
squareMeters vx vy = Vector2d (Area.squareMeters vx) (Area.squareMeters vy)

polar :: Qty units -> Angle -> Vector2d (space @ units)
polar r theta = Vector2d (r * Angle.cos theta) (r * Angle.sin theta)

xComponent :: Vector2d (space @ units) -> Qty units
xComponent (Vector2d vx _) = vx

yComponent :: Vector2d (space @ units) -> Qty units
yComponent (Vector2d _ vy) = vy

{-# INLINE components #-}
components :: Vector2d (space @ units) -> (Qty units, Qty units)
components (Vector2d vx vy) = (vx, vy)

interpolateFrom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Vector2d (space @ units)
interpolateFrom (Vector2d x1 y1) (Vector2d x2 y2) t =
  Vector2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Vector2d (space @ units) -> Vector2d (space @ units) -> Vector2d (space @ units)
midpoint (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

magnitude :: Vector2d (space @ units) -> Qty units
magnitude (Vector2d vx vy) = Qty.hypot2 vx vy

squaredMagnitude :: Units.Squared units1 units2 => Vector2d (space @ units1) -> Qty units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: Vector2d (space @ units) -> Qty (units :*: units)
squaredMagnitude_ (Vector2d vx vy) = Qty.squared_ vx + Qty.squared_ vy

angle :: Vector2d (space @ units) -> Angle
angle (Vector2d vx vy) = Angle.atan2 vy vx

data IsZero = IsZero deriving (Eq, Show, Error)

direction :: Tolerance units => Vector2d (space @ units) -> Result IsZero (Direction2d space)
direction vector =
  let vm = magnitude vector
   in if vm ~= Qty.zero then Error Vector2d.IsZero else Ok (Direction2d.unsafe (vector / vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector2d (space @ units) ->
  Result IsZero (Qty units, Direction2d space)
magnitudeAndDirection vector =
  let vm = magnitude vector
   in if vm ~= Qty.zero then Error Vector2d.IsZero else Ok (vm, Direction2d.unsafe (vector / vm))

normalize :: Vector2d (space @ units) -> Vector2d (space @ Unitless)
normalize vector =
  let vm = magnitude vector
   in if vm == Qty.zero then zero else vector / vm

rotateLeft :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateLeft (Vector2d vx vy) = Vector2d -vy vx

rotateRight :: Vector2d (space @ units) -> Vector2d (space @ units)
rotateRight (Vector2d vx vy) = Vector2d vy -vx

placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis ::
  Basis2d global (Defines local) ->
  Vector2d (local @ units) ->
  Vector2d (global @ units)
placeInBasis basis (Vector2d vx vy) =
  vx * Basis2d.xDirection basis + vy * Basis2d.yDirection basis

relativeToBasis ::
  Basis2d global (Defines local) ->
  Vector2d (global @ units) ->
  Vector2d (local @ units)
relativeToBasis basis vector =
  Vector2d
    (vector <> Basis2d.xDirection basis)
    (vector <> Basis2d.yDirection basis)

sum :: List (Vector2d (space @ units)) -> Vector2d (space @ units)
sum = List.foldl (+) zero
