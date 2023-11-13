module Uv
  ( Parameter (U, V)
  , Space
  , Coordinates
  , Point
  , Direction
  , Bounds
  , domain
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
