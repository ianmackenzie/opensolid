module Axis2d
  ( Axis2d (Axis2d, originPoint, direction)
  , x
  , y
  , through
  )
where

import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Transform2d (Transformable2d (..))

type role Axis2d nominal

data Axis2d (coordinateSystem :: CoordinateSystem) where
  Axis2d ::
    { originPoint :: Point2d (space @ units)
    , direction :: Direction2d space
    } ->
    Axis2d (space @ units)

deriving instance Eq (Axis2d (space @ units))

deriving instance Show (Axis2d (space @ units))

instance (space ~ space', units ~ units') => Transformable2d (Axis2d (space @ units)) (space' @ units') where
  transformBy transformation axis =
    Axis2d
      { originPoint = transformBy transformation (originPoint axis)
      , direction = transformBy transformation (direction axis)
      }

x :: Axis2d (space @ units)
x = Axis2d{originPoint = Point2d.origin, direction = Direction2d.x}

y :: Axis2d (space @ units)
y = Axis2d{originPoint = Point2d.origin, direction = Direction2d.y}

through :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)
through = Axis2d
