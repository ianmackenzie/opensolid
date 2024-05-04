module Axis2d
  ( Axis2d
  , originPoint
  , direction
  , normalDirection
  , x
  , y
  , through
  )
where

import CoordinateSystem (Space)
import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Transform2d (Transformable2d (..))

type role Axis2d phantom

data Axis2d (coordinateSystem :: CoordinateSystem) where
  Axis2d ::
    Point2d coordinateSystem ->
    Direction2d (Space coordinateSystem) ->
    Axis2d coordinateSystem

deriving instance Eq (Axis2d (space @ units))

deriving instance Show (Axis2d (space @ units))

instance (space ~ space', units ~ units') => Transformable2d (Axis2d (space @ units)) (space' @ units') where
  transformBy transformation axis =
    Axis2d
      (transformBy transformation (originPoint axis))
      (transformBy transformation (direction axis))

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
