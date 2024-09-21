module Axis3d
  ( Axis3d
  , originPoint
  , direction
  , x
  , y
  , z
  , through
  , moveTo
  , reverse
  , transformBy
  )
where

import Direction3d (Direction3d)
import Direction3d qualified
import OpenSolid
import Point3d (Point3d)
import Point3d qualified
import Transform qualified
import Transform3d (Transform3d)

data Axis3d (coordinateSystem :: CoordinateSystem) where
  Axis3d ::
    Point3d (space @ units) ->
    Direction3d space ->
    Axis3d (space @ units)

deriving instance Eq (Axis3d (space @ units))

deriving instance Show (Axis3d (space @ units))

originPoint :: Axis3d (space @ units) -> Point3d (space @ units)
originPoint (Axis3d p0 _) = p0

direction :: Axis3d (space @ units) -> Direction3d space
direction (Axis3d _ d) = d

x :: Axis3d (space @ units)
x = Axis3d Point3d.origin Direction3d.x

y :: Axis3d (space @ units)
y = Axis3d Point3d.origin Direction3d.y

z :: Axis3d (space @ units)
z = Axis3d Point3d.origin Direction3d.z

through :: Point3d (space @ units) -> Direction3d space -> Axis3d (space @ units)
through = Axis3d

moveTo :: Point3d (space @ units) -> Axis3d (space @ units) -> Axis3d (space @ units)
moveTo newOriginPoint axis = Axis3d newOriginPoint (direction axis)

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
