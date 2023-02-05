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
data Axis2d coordinates units

originPoint :: Axis2d coordinates units -> Point2d coordinates units
direction :: Axis2d coordinates units -> Direction2d coordinates
