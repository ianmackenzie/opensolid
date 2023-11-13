module Uv
  ( Parameter (U, V)
  , Space
  , Coordinates
  , Point
  , Bounds
  , domain
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import OpenSolid
import Point2d (Point2d)
import Range qualified

data Parameter = U | V deriving (Eq, Show, Ord)

data Space

type Coordinates = Space @ Unitless

type Point = Point2d Coordinates

type Bounds = Bounds2d Coordinates

domain :: Bounds
domain = Bounds2d (Range.from 0.0 1.0) (Range.from 0.0 1.0)
