module Uv
  ( Parameter (U, V)
  , Space
  , Coordinates
  , Point
  , Direction
  , Bounds
  , domain
  , bisect
  , cycle
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)
import Range qualified

data Parameter = U | V deriving (Eq, Show, Ord)

data Space

type Coordinates = Space @ Unitless

type Point = Point2d Coordinates

type Bounds = Bounds2d Coordinates

type Direction = Direction2d Space

domain :: Bounds
domain = Bounds2d (Range.from 0.0 1.0) (Range.from 0.0 1.0)

bisect :: Parameter -> Bounds -> (Bounds, Bounds)
bisect U (Bounds2d u v) = let (u1, u2) = Range.bisect u in (Bounds2d u1 v, Bounds2d u2 v)
bisect V (Bounds2d u v) = let (v1, v2) = Range.bisect v in (Bounds2d u v1, Bounds2d u v2)

cycle :: Parameter -> Parameter
cycle U = V
cycle V = U
