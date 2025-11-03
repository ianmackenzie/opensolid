module OpenSolid.Domain2d
  ( Domain2d (Domain2d)
  , Boundary
  , unit
  , isAtomic
  , coordinates
  , leftEdge
  , rightEdge
  , bottomEdge
  , topEdge
  , bottomLeftCorner
  , bottomRightCorner
  , topLeftCorner
  , topRightCorner
  , adjacent
  , contacts
  , half
  , interior
  , bounds
  , overlaps
  , contains
  , area
  , intersectionArea
  )
where

import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Domain1d (Domain1d)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)

data Domain2d = Domain2d Domain1d Domain1d deriving (Show)

instance HasField "bounds" Domain2d UvBounds where
  getField = bounds

data Boundary
  = Corner Sign Sign Domain1d.Boundary Domain1d.Boundary
  | VerticalEdge Sign Domain1d.Boundary Domain1d
  | HorizontalEdge Sign Domain1d Domain1d.Boundary
  deriving (Show)

unit :: Domain2d
unit = Domain2d Domain1d.unit Domain1d.unit

isAtomic :: Domain2d -> Bool
isAtomic (Domain2d x y) = Domain1d.isAtomic x && Domain1d.isAtomic y

coordinates :: Domain2d -> (Domain1d, Domain1d)
coordinates (Domain2d x y) = (x, y)

leftEdge :: Domain2d -> Boundary
leftEdge (Domain2d x y) = VerticalEdge Negative (Domain1d.lowerBoundary x) y

rightEdge :: Domain2d -> Boundary
rightEdge (Domain2d x y) = VerticalEdge Positive (Domain1d.upperBoundary x) y

bottomEdge :: Domain2d -> Boundary
bottomEdge (Domain2d x y) = HorizontalEdge Negative x (Domain1d.lowerBoundary y)

topEdge :: Domain2d -> Boundary
topEdge (Domain2d x y) = HorizontalEdge Positive x (Domain1d.upperBoundary y)

bottomLeftCorner :: Domain2d -> Boundary
bottomLeftCorner (Domain2d x y) =
  Corner Negative Negative (Domain1d.lowerBoundary x) (Domain1d.lowerBoundary y)

bottomRightCorner :: Domain2d -> Boundary
bottomRightCorner (Domain2d x y) =
  Corner Positive Negative (Domain1d.upperBoundary x) (Domain1d.lowerBoundary y)

topLeftCorner :: Domain2d -> Boundary
topLeftCorner (Domain2d x y) =
  Corner Negative Positive (Domain1d.lowerBoundary x) (Domain1d.upperBoundary y)

topRightCorner :: Domain2d -> Boundary
topRightCorner (Domain2d x y) =
  Corner Positive Positive (Domain1d.upperBoundary x) (Domain1d.upperBoundary y)

adjacent :: Boundary -> Boundary -> Bool
adjacent (Corner xSign1 ySign1 x1 y1) (Corner xSign2 ySign2 x2 y2) =
  xSign1 /= xSign2 && ySign1 /= ySign2 && x1 == x2 && y1 == y2
adjacent (Corner xSign1 _ x1 y1) (VerticalEdge xSign2 x2 y2) =
  xSign1 /= xSign2 && x1 == x2 && Domain1d.includes y1 y2
adjacent (Corner _ ySign1 x1 y1) (HorizontalEdge ySign2 x2 y2) =
  ySign1 /= ySign2 && Domain1d.includes x1 x2 && y1 == y2
adjacent (VerticalEdge xSign1 x1 y1) (Corner xSign2 _ x2 y2) =
  xSign1 /= xSign2 && x1 == x2 && Domain1d.includes y2 y1
adjacent (VerticalEdge xSign1 x1 y1) (VerticalEdge xSign2 x2 y2) =
  xSign1 /= xSign2 && x1 == x2 && Domain1d.overlaps y1 y2
adjacent VerticalEdge{} HorizontalEdge{} =
  False
adjacent (HorizontalEdge ySign1 x1 y1) (Corner _ ySign2 x2 y2) =
  ySign1 /= ySign2 && Domain1d.includes x2 x1 && y1 == y2
adjacent HorizontalEdge{} VerticalEdge{} =
  False
adjacent (HorizontalEdge ySign1 x1 y1) (HorizontalEdge ySign2 x2 y2) =
  ySign1 /= ySign2 && Domain1d.overlaps x1 x2 && y1 == y2

contacts :: Domain2d -> Boundary -> Bool
contacts (Domain2d xDomain yDomain) boundary = case boundary of
  Corner Negative Negative boundaryX boundaryY ->
    (boundaryY == Domain1d.upperBoundary yDomain && Domain1d.includes boundaryX xDomain)
      || (boundaryX == Domain1d.upperBoundary xDomain && Domain1d.includes boundaryY yDomain)
  Corner Positive Negative boundaryX boundaryY ->
    (boundaryY == Domain1d.upperBoundary yDomain && Domain1d.includes boundaryX xDomain)
      || (boundaryX == Domain1d.lowerBoundary xDomain && Domain1d.includes boundaryY yDomain)
  Corner Negative Positive boundaryX boundaryY ->
    (boundaryY == Domain1d.lowerBoundary yDomain && Domain1d.includes boundaryX xDomain)
      || (boundaryX == Domain1d.upperBoundary xDomain && Domain1d.includes boundaryY yDomain)
  Corner Positive Positive boundaryX boundaryY ->
    (boundaryY == Domain1d.lowerBoundary yDomain && Domain1d.includes boundaryX xDomain)
      || (boundaryX == Domain1d.lowerBoundary xDomain && Domain1d.includes boundaryY yDomain)
  VerticalEdge Negative boundaryX boundaryYDomain ->
    boundaryX == Domain1d.upperBoundary xDomain && Domain1d.overlaps yDomain boundaryYDomain
  VerticalEdge Positive boundaryX boundaryYDomain ->
    boundaryX == Domain1d.lowerBoundary xDomain && Domain1d.overlaps yDomain boundaryYDomain
  HorizontalEdge Negative boundaryXDomain boundaryY ->
    boundaryY == Domain1d.upperBoundary yDomain && Domain1d.overlaps xDomain boundaryXDomain
  HorizontalEdge Positive boundaryXDomain boundaryY ->
    boundaryY == Domain1d.lowerBoundary yDomain && Domain1d.overlaps xDomain boundaryXDomain

half :: Domain2d -> Domain2d
half (Domain2d x y) = Domain2d (Domain1d.half x) (Domain1d.half y)

bounds :: Domain2d -> UvBounds
bounds (Domain2d x y) = Bounds2d x.bounds y.bounds

interior :: Domain2d -> UvBounds
interior (Domain2d x y) = Bounds2d (Domain1d.interior x) (Domain1d.interior y)

overlaps :: Domain2d -> Domain2d -> Bool
overlaps (Domain2d x2 y2) (Domain2d x1 y1) = Domain1d.overlaps x2 x1 && Domain1d.overlaps y2 y1

contains :: Domain2d -> Domain2d -> Bool
contains (Domain2d x2 y2) (Domain2d x1 y1) = Domain1d.contains x2 x1 && Domain1d.contains y2 y1

area :: Domain2d -> Number
area (Domain2d x y) = Domain1d.width x * Domain1d.width y

intersectionArea :: Domain2d -> Domain2d -> Number
intersectionArea (Domain2d x1 y1) (Domain2d x2 y2) =
  Domain1d.intersectionWidth x1 x2 * Domain1d.intersectionWidth y1 y2
