module OpenSolid.Axis2d
  ( Axis2d (Axis2d, originPoint, direction)
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
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2d (Axis2d, direction, originPoint)
  , Axis3d (Axis3d)
  , Plane3d
  , Transform2d
  )
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2d qualified as Transform2d

-- | Get the origin point of an axis.
originPoint :: Axis2d units space -> Point2D units space
originPoint (Axis2d p0 _) = p0

-- | Get the direction of an axis.
direction :: Axis2d units space -> Direction2d space
direction (Axis2d _ d) = d

leftwardDirection :: Axis2d units space -> Direction2d space
leftwardDirection axis = Direction2d.rotateLeft (direction axis)

rightwardDirection :: Axis2d units space -> Direction2d space
rightwardDirection axis = Direction2d.rotateRight (direction axis)

-- | The X axis.
x :: Axis2d units space
x = Axis2d Point2D.origin Direction2d.x

-- | The Y axis.
y :: Axis2d units space
y = Axis2d Point2D.origin Direction2d.y

through :: Point2D units space -> Direction2d space -> Axis2d units space
through = Axis2d

moveTo :: Point2D units space -> Axis2d units space -> Axis2d units space
moveTo newOriginPoint axis = Axis2d newOriginPoint (direction axis)

reverse :: Axis2d units space -> Axis2d units space
reverse (Axis2d p0 d) = Axis2d p0 (negative d)

{-| Convert a 2D axis to 3D axis by placing it on a plane.

Given a 2D axis defined within a plane's coordinate system,
this returns the corresponding 3D axis.
-}
placeOn :: Plane3d global local -> Axis2d Meters local -> Axis3d global
placeOn plane (Axis2d p0 d) = Axis3d (Point2D.placeOn plane p0) (Direction2d.placeOn plane d)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2d tag units space ->
  Axis2d units space ->
  Axis2d units space
transformBy transform axis = do
  let transformedOriginPoint = Point2D.transformBy transform (originPoint axis)
  let transformedDirection = Direction2d.transformBy transform (direction axis)
  Axis2d transformedOriginPoint transformedDirection

offsetLeftwardBy :: Quantity units -> Axis2d units space -> Axis2d units space
offsetLeftwardBy distance axis = translateIn (leftwardDirection axis) distance axis

offsetRightwardBy :: Quantity units -> Axis2d units space -> Axis2d units space
offsetRightwardBy distance axis = translateIn (rightwardDirection axis) distance axis

translateBy ::
  Vector2D units space ->
  Axis2d units space ->
  Axis2d units space
translateBy = Transform2d.translateByImpl transformBy

translateIn ::
  Direction2d space ->
  Quantity units ->
  Axis2d units space ->
  Axis2d units space
translateIn = Transform2d.translateInImpl transformBy

translateAlong ::
  Axis2d units space ->
  Quantity units ->
  Axis2d units space ->
  Axis2d units space
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround ::
  Point2D units space ->
  Angle ->
  Axis2d units space ->
  Axis2d units space
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2d units space ->
  Axis2d units space ->
  Axis2d units space
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy
