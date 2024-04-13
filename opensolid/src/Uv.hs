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

import Bounds2d (Bounds2d)
import Bounds2d qualified
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
domain = Bounds2d.xy Range.unit Range.unit

bisect :: Parameter -> Bounds -> (Bounds, Bounds)
bisect U bounds = do
  let (uRange, vRange) = Bounds2d.xyRanges bounds
  let (u1, u2) = Range.bisect uRange
  (Bounds2d.xy u1 vRange, Bounds2d.xy u2 vRange)
bisect V bounds = do
  let (uRange, vRange) = Bounds2d.xyRanges bounds
  let (v1, v2) = Range.bisect vRange
  (Bounds2d.xy uRange v1, Bounds2d.xy uRange v2)

cycle :: Parameter -> Parameter
cycle U = V
cycle V = U
