module OpenSolid.Vector3d
  ( Vector3d (Vector3d)
  , zero
  , unit
  , x
  , y
  , z
  , xy
  , xz
  , yz
  , xyz
  , xyzIn
  , xyzInBasis
  , meters
  , centimeters
  , millimeters
  , inches
  , squareMeters
  , xComponent
  , yComponent
  , zComponent
  , componentIn
  , projectionIn
  , components
  , midpoint
  , interpolateFrom
  , magnitude
  , squaredMagnitude
  , squaredMagnitude'
  , IsZero (IsZero)
  , direction
  , magnitudeAndDirection
  , normalize
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , sum
  , transformBy
  , rotateIn
  , scaleIn
  , scaleAbout
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Area qualified as Area
import OpenSolid.Error qualified as Error
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import {-# SOURCE #-} OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Basis3d (Basis3d)
  , Direction3d (Unit3d)
  , Frame3d (Frame3d)
  , Point3d
  , Vector3d (Vector3d)
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units (Meters, SquareMeters)
import OpenSolid.Units qualified as Units

-- | The zero vector.
zero :: Vector3d (space @ units)
zero = Vector3d Qty.zero Qty.zero Qty.zero

-- | Construct a unit vector in the given direction.
unit :: Direction3d space -> Vector3d (space @ Unitless)
unit (Unit3d vector) = vector

{-| Construct a vector from just an X component.

The Y and Z components will be set to zero.
-}
x :: Qty units -> Vector3d (space @ units)
x vx = Vector3d vx Qty.zero Qty.zero

{-| Construct a vector from just a Y component.

The X and Z components will be set to zero.
-}
y :: Qty units -> Vector3d (space @ units)
y vy = Vector3d Qty.zero vy Qty.zero

{-| Construct a vector from just a Z component.

The X and Y components will be set to zero.
-}
z :: Qty units -> Vector3d (space @ units)
z vz = Vector3d Qty.zero Qty.zero vz

{-| Construct a vector from X and Y components.

The Z component will be set to zero.
-}
xy :: Qty units -> Qty units -> Vector3d (space @ units)
xy vx vz = Vector3d vx vz Qty.zero

{-| Construct a vector from X and Z components.

The Y component will be set to zero.
-}
xz :: Qty units -> Qty units -> Vector3d (space @ units)
xz vx vz = Vector3d vx Qty.zero vz

{-| Construct a vector from Y and Z components.

The X component will be set to zero.
-}
yz :: Qty units -> Qty units -> Vector3d (space @ units)
yz vy vz = Vector3d Qty.zero vy vz

-- | Construct a vector from its X, Y and Z components.
xyz :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)
xyz = Vector3d

xyzIn ::
  Frame3d (space @ originUnits) defines ->
  Qty units ->
  Qty units ->
  Qty units ->
  Vector3d (space @ units)
xyzIn (Frame3d _ basis) = xyzInBasis basis

xyzInBasis ::
  Basis3d space defines ->
  Qty units ->
  Qty units ->
  Qty units ->
  Vector3d (space @ units)
xyzInBasis (Basis3d i j k) vx vy vz = vx * i + vy * j + vz * k

apply :: (Float -> Qty units) -> Float -> Float -> Float -> Vector3d (space @ units)
apply units px py pz = Vector3d (units px) (units py) (units pz)

-- | Construct a vector from its XYZ components given in meters.
meters :: Float -> Float -> Float -> Vector3d (space @ Meters)
meters = apply Length.meters

-- | Construct a vector from its XYZ components given in centimeters.
centimeters :: Float -> Float -> Float -> Vector3d (space @ Meters)
centimeters = apply Length.centimeters

-- | Construct a vector from its XYZ components given in millimeters.
millimeters :: Float -> Float -> Float -> Vector3d (space @ Meters)
millimeters = apply Length.millimeters

-- | Construct a vector from its XYZ components given in inches.
inches :: Float -> Float -> Float -> Vector3d (space @ Meters)
inches = apply Length.inches

-- | Construct a vector from its XYZ components given in square meters.
squareMeters :: Float -> Float -> Float -> Vector3d (space @ SquareMeters)
squareMeters vx vy vz =
  Vector3d (Area.squareMeters vx) (Area.squareMeters vy) (Area.squareMeters vz)

-- | Get the X component of a vector.
xComponent :: Vector3d (space @ units) -> Qty units
xComponent (Vector3d vx _ _) = vx

-- | Get the Y component of a vector.
yComponent :: Vector3d (space @ units) -> Qty units
yComponent (Vector3d _ vy _) = vy

-- | Get the Z component of a vector.
zComponent :: Vector3d (space @ units) -> Qty units
zComponent (Vector3d _ _ vz) = vz

componentIn :: Direction3d space -> Vector3d (space @ units) -> Qty units
componentIn = (<>)

projectionIn :: Direction3d space -> Vector3d (space @ units) -> Vector3d (space @ units)
projectionIn givenDirection vector = givenDirection * componentIn givenDirection vector

-- | Get the XYZ components of a vector as a tuple.
{-# INLINE components #-}
components :: Vector3d (space @ units) -> (Qty units, Qty units, Qty units)
components (Vector3d vx vy vz) = (vx, vy, vz)

interpolateFrom ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Float ->
  Vector3d (space @ units)
interpolateFrom (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) t =
  Vector3d
    (x1 + t * (x2 - x1))
    (y1 + t * (y2 - y1))
    (z1 + t * (z2 - z1))

midpoint :: Vector3d (space @ units) -> Vector3d (space @ units) -> Vector3d (space @ units)
midpoint (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  Vector3d (0.5 * (x1 + x2)) (0.5 * (y1 + y2)) (0.5 * (z1 + z2))

magnitude :: Vector3d (space @ units) -> Qty units
magnitude (Vector3d vx vy vz) = Qty.hypot3 vx vy vz

squaredMagnitude :: Units.Squared units1 units2 => Vector3d (space @ units1) -> Qty units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: Vector3d (space @ units) -> Qty (units :*: units)
squaredMagnitude' (Vector3d vx vy vz) = vx .*. vx + vy .*. vy + vz .*. vz

data IsZero = IsZero deriving (Eq, Show, Error.Message)

{-| Attempt to get the direction of a vector.

The current tolerance will be used to check if the vector is zero
(and therefore does not have a direction).
-}
direction :: Tolerance units => Vector3d (space @ units) -> Result IsZero (Direction3d space)
direction vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Failure IsZero else Success (Unit3d (vector / vm))

magnitudeAndDirection ::
  Tolerance units =>
  Vector3d (space @ units) ->
  Result IsZero (Qty units, Direction3d space)
magnitudeAndDirection vector = do
  let vm = magnitude vector
  if vm ~= Qty.zero then Failure IsZero else Success (vm, Unit3d (vector / vm))

normalize :: Vector3d (space @ units) -> Vector3d (space @ Unitless)
normalize vector = do
  let vm = magnitude vector
  if vm == Qty.zero then zero else vector / vm

placeIn ::
  Frame3d (global @ originUnits) (Defines local) ->
  Vector3d (local @ units) ->
  Vector3d (global @ units)
placeIn (Frame3d _ basis) vector = placeInBasis basis vector

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  Vector3d (global @ units) ->
  Vector3d (local @ units)
relativeTo (Frame3d _ basis) vector = relativeToBasis basis vector

placeInBasis ::
  Basis3d global (Defines local) ->
  Vector3d (local @ units) ->
  Vector3d (global @ units)
placeInBasis (Basis3d i j k) (Vector3d vx vy vz) = vx * i + vy * j + vz * k

relativeToBasis ::
  Basis3d global (Defines local) ->
  Vector3d (global @ units) ->
  Vector3d (local @ units)
relativeToBasis (Basis3d i j k) vector = Vector3d (vector <> i) (vector <> j) (vector <> k)

sum :: List (Vector3d (space @ units)) -> Vector3d (space @ units)
sum = List.foldl (+) zero

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
transformBy transform vector = do
  let Transform3d _ i j k = transform
  let Vector3d vx vy vz = vector
  vx * i + vy * j + vz * k

rotateIn :: Direction3d space -> Angle -> Vector3d (space @ units) -> Vector3d (space @ units)
rotateIn axisDirection = rotateAround (Axis3d Point3d.origin axisDirection)

scaleIn :: Direction3d space -> Float -> Vector3d (space @ units) -> Vector3d (space @ units)
scaleIn axisDirection = scaleAlong (Axis3d Point3d.origin axisDirection)

rotateAround ::
  Axis3d (space @ axisUnits) ->
  Angle ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
rotateAround = Transform3d.rotateAroundImpl transformBy

scaleAbout ::
  Point3d (space @ pointUnits) ->
  Float ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
scaleAbout = Transform3d.scaleAboutImpl transformBy

scaleAlong ::
  Axis3d (space @ axisUnits) ->
  Float ->
  Vector3d (space @ units) ->
  Vector3d (space @ units)
scaleAlong = Transform3d.scaleAlongImpl transformBy
