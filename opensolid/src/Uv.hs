module Uv (Space, Coordinates, sample) where

import Domain (Domain)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import {-# SOURCE #-} Point2d qualified
import Quadrature qualified
import Range qualified

data Space

type Coordinates = Space @ Unitless

sample :: (Point2d Coordinates -> a) -> Domain -> Domain -> List a
sample function u v =
  [ function (Point2d.xy (Range.interpolate u Quadrature.t1) (Range.interpolate v Quadrature.t1))
  , function (Point2d.xy (Range.interpolate u Quadrature.t2) (Range.interpolate v Quadrature.t5))
  , function (Point2d.xy (Range.interpolate u Quadrature.t3) (Range.interpolate v Quadrature.t3))
  , function (Point2d.xy (Range.interpolate u Quadrature.t4) (Range.interpolate v Quadrature.t4))
  , function (Point2d.xy (Range.interpolate u Quadrature.t5) (Range.interpolate v Quadrature.t2))
  ]
