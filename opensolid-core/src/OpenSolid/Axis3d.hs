module OpenSolid.Axis3d
  ( Axis3d (Axis3d)
  , originPoint
  , direction
  , x
  , y
  , z
  , through
  , normalPlane
  , moveTo
  , reverse
  , transformBy
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import {-# SOURCE #-} OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3d (Axis3d), Plane3d)
import OpenSolid.Transform qualified as Transform
import OpenSolid.Transform3d (Transform3d)

-- | Get the origin point of an axis.
originPoint :: Axis3d (space @ units) -> Point3d (space @ units)
originPoint (Axis3d p0 _) = p0

-- | Get the direction of an axis.
direction :: Axis3d (space @ units) -> Direction3d space
direction (Axis3d _ d) = d

-- | The global X axis.
x :: Axis3d (space @ units)
x = Axis3d Point3d.origin Direction3d.x

-- | The global Y axis.
y :: Axis3d (space @ units)
y = Axis3d Point3d.origin Direction3d.y

-- | The global Z axis.
z :: Axis3d (space @ units)
z = Axis3d Point3d.origin Direction3d.z

through :: Point3d (space @ units) -> Direction3d space -> Axis3d (space @ units)
through = Axis3d

{-| Construct a plane normal (perpendicular) to the given axis.

The origin point of the plane will be the origin point of the axis,
and the normal direction of the plane will be the direction of the axis.
The X and Y directions of the plane will be chosen arbitrarily.
-}
normalPlane :: Axis3d (space @ units) -> Plane3d (space @ units) defines
normalPlane (Axis3d p0 d) = Plane3d.through p0 d

{-| Move an axis so that its origin point is the given point.

The direction of the axis will remain unchanged.
-}
moveTo :: Point3d (space @ units) -> Axis3d (space @ units) -> Axis3d (space @ units)
moveTo newOriginPoint axis = Axis3d newOriginPoint (direction axis)

{-| Reverse an axis (negate/reverse its direction).

The origin point of the axis will remain unchanged.
-}
reverse :: Axis3d (space @ units) -> Axis3d (space @ units)
reverse (Axis3d p0 d) = Axis3d p0 -d

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform3d tag (space @ units) ->
  Axis3d (space @ units) ->
  Axis3d (space @ units)
transformBy transform axis = do
  let transformedOriginPoint = Point3d.transformBy transform (originPoint axis)
  let transformedDirection = Direction3d.transformBy transform (direction axis)
  Axis3d transformedOriginPoint transformedDirection
