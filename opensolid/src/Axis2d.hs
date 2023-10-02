module Axis2d
  ( Axis2d (Axis2d)
  , originPoint
  , direction
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
  Axis2d :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)

deriving instance Eq (Axis2d (space @ units))

deriving instance Show (Axis2d (space @ units))

instance (space ~ space', units ~ units') => Transformable2d (Axis2d (space @ units)) (space' @ units') where
  transformBy transformation axis =
    Axis2d
      (transformBy transformation (originPoint axis))
      (transformBy transformation (direction axis))

{-# INLINE originPoint #-}
originPoint :: Axis2d (space @ units) -> Point2d (space @ units)
originPoint (Axis2d p _) = p

{-# INLINE direction #-}
direction :: Axis2d (space @ units) -> Direction2d space
direction (Axis2d _ d) = d

x :: Axis2d (space @ units)
x = Axis2d Point2d.origin Direction2d.x

y :: Axis2d (space @ units)
y = Axis2d Point2d.origin Direction2d.y

through :: Point2d (space @ units) -> Direction2d space -> Axis2d (space @ units)
through = Axis2d
