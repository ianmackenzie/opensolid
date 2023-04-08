module Axis2d
  ( Axis2d (originPoint, direction)
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

data Axis2d (coordinateSystem :: CoordinateSystem) = Axis2d
  { originPoint :: Point2d coordinateSystem
  , direction :: Direction2d (Space coordinateSystem)
  }
  deriving (Eq, Show)

instance (space ~ space', units ~ units') => Transformable2d (Axis2d (space @ units)) (space' @ units') where
  transformBy transformation axis =
    Axis2d
      (transformBy transformation (originPoint axis))
      (transformBy transformation (direction axis))

x :: Axis2d (space @ units)
x = Axis2d Point2d.origin Direction2d.x

y :: Axis2d (space @ units)
y = Axis2d Point2d.origin Direction2d.y

through :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)
through = Axis2d
