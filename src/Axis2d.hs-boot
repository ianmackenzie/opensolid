module Axis2d
  ( Axis2d
  , direction
  , originPoint
  )
where

import Direction2d (Direction2d)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)

type role Axis2d nominal nominal

type Axis2d :: Type -> Type -> Type
data Axis2d units coordinates

originPoint :: Axis2d units coordinates -> Point2d units coordinates
direction :: Axis2d units coordinates -> Direction2d coordinates
