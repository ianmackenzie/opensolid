module Axis2d
  ( Axis2d (originPoint, direction)
  )
where

import Direction2d (Direction2d)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)

type role Axis2d nominal nominal

type Axis2d :: Type -> Type -> Type
data Axis2d coordinates units = Axis2d
  { originPoint :: Point2d coordinates units
  , direction :: Direction2d coordinates
  }
