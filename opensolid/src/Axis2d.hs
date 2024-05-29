module Axis2d
  ( Axis2d
  , originPoint
  , direction
  , normalDirection
  , x
  , y
  , through
  , moveTo
  , transformBy
  , translateBy
  , translateIn
  , translateInOwn
  , translateAlong
  , rotateAround
  , rotateAroundOwn
  , offsetBy
  )
where

import CoordinateSystem (Space)
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Vector2d (Vector2d)

type role Axis2d phantom

data Axis2d (coordinateSystem :: CoordinateSystem) where
  Axis2d ::
    Point2d coordinateSystem ->
    Direction2d (Space coordinateSystem) ->
    Axis2d coordinateSystem

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

transformBy ::
  Transform2d.IsOrthonormal a =>
  Transform2d a (space @ units) ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
transformBy transform axis = do
  let transformedOriginPoint = Point2d.transformBy transform (originPoint axis)
  let transformedDirection = Direction2d.transformBy transform (direction axis)
  Axis2d transformedOriginPoint transformedDirection

translateBy :: Vector2d (space @ units) -> Axis2d (space @ units) -> Axis2d (space @ units)
translateBy displacement axis =
  Axis2d (Point2d.translateBy displacement (originPoint axis)) (direction axis)

translateIn :: Direction2d space -> Qty units -> Axis2d (space @ units) -> Axis2d (space @ units)
translateIn givenDirection distance = translateBy (givenDirection * distance)

translateInOwn ::
  (Axis2d (space @ units) -> Direction2d space) ->
  Qty units ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateInOwn getDirection distance axis = translateIn (getDirection axis) distance axis

offsetBy :: Qty units -> Axis2d (space @ units) -> Axis2d (space @ units)
offsetBy distance = translateInOwn normalDirection distance

translateAlong ::
  Axis2d (space @ units) ->
  Qty units ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
translateAlong otherAxis distance = transformBy (Transform2d.translateAlong otherAxis distance)

rotateAround :: Point2d (space @ units) -> Angle -> Axis2d (space @ units) -> Axis2d (space @ units)
rotateAround centerPoint angle = transformBy (Transform2d.rotateAround centerPoint angle)

rotateAroundOwn ::
  (Axis2d (space @ units) -> Point2d (space @ units)) ->
  Angle ->
  Axis2d (space @ units) ->
  Axis2d (space @ units)
rotateAroundOwn getCenterPoint angle axis = rotateAround (getCenterPoint axis) angle axis
