module Axis2d
  ( Axis2d
  , originPoint
  , direction
  , normalDirection
  , x
  , y
  , through
  , moveTo
  , reverse
  , transformBy
  , translateBy
  , translateByOwn
  , translateIn
  , translateInOwn
  , translateAlong
  , translateAlongOwn
  , rotateAround
  , rotateAroundOwn
  , mirrorAcross
  , mirrorAcrossOwn
  , offsetBy
  )
where

import Angle (Angle)
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Transform qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Vector2d (Vector2d)

data Axis2d (coordinateSystem :: CoordinateSystem) where
  Axis2d ::
    Point2d (space @ units) ->
    Direction2d space ->
    Axis2d (space @ units)

deriving instance Eq (Axis2d (space @ units))

deriving instance Show (Axis2d (space @ units))

originPoint :: Axis2d (space @ units) -> Point2d (space @ units)
originPoint (Axis2d p0 _) = p0

direction :: Axis2d (space @ units) -> Direction2d space
direction (Axis2d _ d) = d

normalDirection :: Axis2d (space @ units) -> Direction2d space
normalDirection axis = Direction2d.perpendicularTo (direction axis)

x :: Axis2d (space @ units)
x = Axis2d Point2d.origin Direction2d.x

y :: Axis2d (space @ units)
y = Axis2d Point2d.origin Direction2d.y

through :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)
through = Axis2d

moveTo :: Point2d (space @ units) -> Axis2d (space @ units) -> Axis2d (space @ units)
moveTo newOriginPoint axis = Axis2d newOriginPoint (direction axis)

reverse :: Axis2d (space @ units) -> Axis2d (space @ units)
reverse (Axis2d p0 d) = Axis2d p0 -d

transformBy ::
  Transform.IsOrthonormal tag =>
  Transform2d tag (space @ units) ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
transformBy transform axis = do
  let transformedOriginPoint = Point2d.transformBy transform (originPoint axis)
  let transformedDirection = Direction2d.transformBy transform (direction axis)
  Axis2d transformedOriginPoint transformedDirection

offsetBy :: Qty units -> Axis2d (space @ units) -> Axis2d (space @ units)
offsetBy distance = translateInOwn normalDirection distance

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

translateByOwn ::
  (Axis2d (space @ units) -> Vector2d (space @ units)) ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateByOwn = Transform2d.translateByOwnImpl transformBy

translateInOwn ::
  (Axis2d (space @ units) -> Direction2d space) ->
  Qty units ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateInOwn = Transform2d.translateInOwnImpl transformBy

translateAlongOwn ::
  (Axis2d (space @ units) -> Axis2d (space @ units)) ->
  Qty units ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateAlongOwn = Transform2d.translateAlongOwnImpl transformBy

rotateAroundOwn ::
  (Axis2d (space @ units) -> Point2d (space @ units)) ->
  Angle ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
rotateAroundOwn = Transform2d.rotateAroundOwnImpl transformBy

mirrorAcrossOwn ::
  (Axis2d (space @ units) -> Axis2d (space @ units)) ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
mirrorAcrossOwn = Transform2d.mirrorAcrossOwnImpl transformBy
