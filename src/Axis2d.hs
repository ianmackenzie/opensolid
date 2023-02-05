module Axis2d
  ( Axis2d
  , through
  , direction
  , originPoint
  )
where

import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)

type role Axis2d nominal nominal

type Axis2d :: Type -> Type -> Type
data Axis2d units coordinates = Axis2d
  { originPoint :: Point2d units coordinates
  , direction :: Direction2d coordinates
  }

through :: Point2d units coordinates -> Direction2d coordinates -> Axis2d units coordinates
through = Axis2d
