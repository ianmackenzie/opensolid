module Uv
  ( Parameter (U, V)
  , Space
  , Coordinates
  , Point
  , Direction
  , Bounds
  , domain
  , samples
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Quadrature qualified
import Range qualified

data Parameter = U | V deriving (Eq, Show, Ord)

data Space

type Coordinates = Space @ Unitless

type Point = Point2d Coordinates

type Bounds = Bounds2d Coordinates

type Direction = Direction2d Space

domain :: Bounds
domain = Bounds2d.xy Range.unit Range.unit

samples :: List Point
samples = do
  let (t1, t2, t3, t4) = Quadrature.abscissae4
  [Point2d.xy t1 t3, Point2d.xy t2 t1, Point2d.xy t3 t4, Point2d.xy t4 t2]
