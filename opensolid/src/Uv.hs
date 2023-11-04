module Uv (Space, Coordinates, Point, Bounds, domain) where

import {-# SOURCE #-} Bounds2d (Bounds2d)
import {-# SOURCE #-} Bounds2d qualified
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)
import Range qualified

data Space

type Coordinates = Space @ Unitless

type Point = Point2d Coordinates

type Bounds = Bounds2d Coordinates

domain :: Bounds
domain = Bounds2d.xy (Range.from 0.0 1.0) (Range.from 0.0 1.0)
