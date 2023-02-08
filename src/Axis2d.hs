module Axis2d
  ( Axis2d
  , x
  , y
  , through
  , direction
  , originPoint
  )
where

import Direction2d (Direction2d)
import Direction2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Transform2d (NoScale, Transformation2d (..))

type role Axis2d nominal nominal

type Axis2d :: Type -> Type -> Type
data Axis2d coordinates units = Axis2d
  { originPoint :: Point2d coordinates units
  , direction :: Direction2d coordinates
  }
  deriving (Eq, Show)

instance
  (coordinates ~ coordinates', units ~ units')
  => Transformation2d (Axis2d coordinates units) NoScale coordinates' units'
  where
  apply transformation axis =
    Axis2d
      (apply transformation (originPoint axis))
      (apply transformation (direction axis))

x :: Axis2d coordinates units
x = Axis2d Point2d.origin Direction2d.x

y :: Axis2d coordinates units
y = Axis2d Point2d.origin Direction2d.y

through :: Point2d coordinates units -> Direction2d coordinates -> Axis2d coordinates units
through = Axis2d
