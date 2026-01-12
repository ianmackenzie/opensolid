module OpenSolid.Axis2D
  ( Axis2D (Axis2D, originPoint, direction)
  , originPoint
  , direction
  , leftwardDirection
  , rightwardDirection
  , x
  , y
  , through
  , moveTo
  , reverse
  , placeOn
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , offsetLeftwardBy
  , offsetRightwardBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2D (Axis2D, direction, originPoint)
  , Axis3D (Axis3D)
  , Plane3D
  , Transform2D
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2D qualified as Transform2D

-- | Get the origin point of an axis.
originPoint :: Axis2D units space -> Point2D units space
originPoint (Axis2D p0 _) = p0

-- | Get the direction of an axis.
direction :: Axis2D units space -> Direction2D space
direction (Axis2D _ d) = d

leftwardDirection :: Axis2D units space -> Direction2D space
leftwardDirection axis = Direction2D.rotateLeft (direction axis)

rightwardDirection :: Axis2D units space -> Direction2D space
rightwardDirection axis = Direction2D.rotateRight (direction axis)

-- | The X axis.
x :: Axis2D units space
x = Axis2D Point2D.origin Direction2D.x

-- | The Y axis.
y :: Axis2D units space
y = Axis2D Point2D.origin Direction2D.y

through :: Point2D units space -> Direction2D space -> Axis2D units space
through = Axis2D

moveTo :: Point2D units space -> Axis2D units space -> Axis2D units space
moveTo newOriginPoint axis = Axis2D newOriginPoint (direction axis)

reverse :: Axis2D units space -> Axis2D units space
reverse (Axis2D p0 d) = Axis2D p0 (negative d)

{-| Convert a 2D axis to 3D axis by placing it on a plane.

Given a 2D axis defined within a plane's coordinate system,
this returns the corresponding 3D axis.
-}
placeOn :: Plane3D global local -> Axis2D Meters local -> Axis3D global
placeOn plane (Axis2D p0 d) = Axis3D (Point2D.placeOn plane p0) (Direction2D.placeOn plane d)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2D tag units space ->
  Axis2D units space ->
  Axis2D units space
transformBy transform axis = do
  let transformedOriginPoint = Point2D.transformBy transform (originPoint axis)
  let transformedDirection = Direction2D.transformBy transform (direction axis)
  Axis2D transformedOriginPoint transformedDirection

offsetLeftwardBy :: Quantity units -> Axis2D units space -> Axis2D units space
offsetLeftwardBy distance axis = translateIn (leftwardDirection axis) distance axis

offsetRightwardBy :: Quantity units -> Axis2D units space -> Axis2D units space
offsetRightwardBy distance axis = translateIn (rightwardDirection axis) distance axis

translateBy ::
  Vector2D units space ->
  Axis2D units space ->
  Axis2D units space
translateBy = Transform2D.translateByImpl transformBy

translateIn ::
  Direction2D space ->
  Quantity units ->
  Axis2D units space ->
  Axis2D units space
translateIn = Transform2D.translateInImpl transformBy

translateAlong ::
  Axis2D units space ->
  Quantity units ->
  Axis2D units space ->
  Axis2D units space
translateAlong = Transform2D.translateAlongImpl transformBy

rotateAround ::
  Point2D units space ->
  Angle ->
  Axis2D units space ->
  Axis2D units space
rotateAround = Transform2D.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2D units space ->
  Axis2D units space ->
  Axis2D units space
mirrorAcross = Transform2D.mirrorAcrossImpl transformBy
