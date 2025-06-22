module OpenSolid.Axis2d
  ( Axis2d (Axis2d)
  , originPoint
  , direction
  , leftwardDirection
  , rightwardDirection
  , x
  , y
  , through
  , moveTo
  , reverse
  , on
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
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis2d (Axis2d), Axis3d (Axis3d), Plane3d, Transform2d)
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Vector2d (Vector2d)

-- | Get the origin point of an axis.
originPoint :: Axis2d (space @ units) -> Point2d (space @ units)
originPoint (Axis2d p0 _) = p0

-- | Get the direction of an axis.
direction :: Axis2d (space @ units) -> Direction2d space
direction (Axis2d _ d) = d

leftwardDirection :: Axis2d (space @ units) -> Direction2d space
leftwardDirection axis = Direction2d.rotateLeft (direction axis)

rightwardDirection :: Axis2d (space @ units) -> Direction2d space
rightwardDirection axis = Direction2d.rotateRight (direction axis)

-- | The X axis.
x :: Axis2d (space @ units)
x = Axis2d Point2d.origin Direction2d.x

-- | The Y axis.
y :: Axis2d (space @ units)
y = Axis2d Point2d.origin Direction2d.y

through :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)
through = Axis2d

moveTo :: Point2d (space @ units) -> Axis2d (space @ units) -> Axis2d (space @ units)
moveTo newOriginPoint axis = Axis2d newOriginPoint (direction axis)

reverse :: Axis2d (space @ units) -> Axis2d (space @ units)
reverse (Axis2d p0 d) = Axis2d p0 -d

on :: Plane3d (space @ units) (Defines local) -> Axis2d (local @ units) -> Axis3d (space @ units)
on plane (Axis2d p0 d) = Axis3d (Point2d.on plane p0) (Direction2d.on plane d)

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2d tag (space @ units) ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
transformBy transform axis = do
  let transformedOriginPoint = Point2d.transformBy transform (originPoint axis)
  let transformedDirection = Direction2d.transformBy transform (direction axis)
  Axis2d transformedOriginPoint transformedDirection

offsetLeftwardBy :: Qty units -> Axis2d (space @ units) -> Axis2d (space @ units)
offsetLeftwardBy distance axis = axis |> translateIn (leftwardDirection axis) distance

offsetRightwardBy :: Qty units -> Axis2d (space @ units) -> Axis2d (space @ units)
offsetRightwardBy distance axis = axis |> translateIn (rightwardDirection axis) distance

translateBy ::
  Vector2d (space @ units) ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateBy = Transform2d.translateByImpl transformBy

translateIn ::
  Direction2d space ->
  Qty units ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateIn = Transform2d.translateInImpl transformBy

translateAlong ::
  Axis2d (space @ units) ->
  Qty units ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround ::
  Point2d (space @ units) ->
  Angle ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2d (space @ units) ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy
