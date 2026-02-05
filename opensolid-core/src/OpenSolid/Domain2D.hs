module OpenSolid.Domain2D
  ( Domain2D (Domain2D)
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

import GHC.Records (HasField)
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Domain1D (Domain1D)
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)

data Domain2D = Domain2D Domain1D Domain1D deriving (Show)

instance HasField "bounds" Domain2D UvBounds where
  getField = bounds

data Boundary
  = Corner Sign Sign Domain1D.Boundary Domain1D.Boundary
  | VerticalEdge Sign Domain1D.Boundary Domain1D
  | HorizontalEdge Sign Domain1D Domain1D.Boundary
  deriving (Show)

unit :: Domain2D
unit = Domain2D Domain1D.unit Domain1D.unit

isAtomic :: Domain2D -> Bool
isAtomic (Domain2D x y) = Domain1D.isAtomic x && Domain1D.isAtomic y

coordinates :: Domain2D -> (Domain1D, Domain1D)
coordinates (Domain2D x y) = (x, y)

leftEdge :: Domain2D -> Boundary
leftEdge (Domain2D x y) = VerticalEdge Negative (Domain1D.lowerBoundary x) y

rightEdge :: Domain2D -> Boundary
rightEdge (Domain2D x y) = VerticalEdge Positive (Domain1D.upperBoundary x) y

bottomEdge :: Domain2D -> Boundary
bottomEdge (Domain2D x y) = HorizontalEdge Negative x (Domain1D.lowerBoundary y)

topEdge :: Domain2D -> Boundary
topEdge (Domain2D x y) = HorizontalEdge Positive x (Domain1D.upperBoundary y)

bottomLeftCorner :: Domain2D -> Boundary
bottomLeftCorner (Domain2D x y) =
  Corner Negative Negative (Domain1D.lowerBoundary x) (Domain1D.lowerBoundary y)

bottomRightCorner :: Domain2D -> Boundary
bottomRightCorner (Domain2D x y) =
  Corner Positive Negative (Domain1D.upperBoundary x) (Domain1D.lowerBoundary y)

topLeftCorner :: Domain2D -> Boundary
topLeftCorner (Domain2D x y) =
  Corner Negative Positive (Domain1D.lowerBoundary x) (Domain1D.upperBoundary y)

topRightCorner :: Domain2D -> Boundary
topRightCorner (Domain2D x y) =
  Corner Positive Positive (Domain1D.upperBoundary x) (Domain1D.upperBoundary y)

adjacent :: Boundary -> Boundary -> Bool
adjacent (Corner xSign1 ySign1 x1 y1) (Corner xSign2 ySign2 x2 y2) =
  xSign1 /= xSign2 && ySign1 /= ySign2 && x1 == x2 && y1 == y2
adjacent (Corner xSign1 _ x1 y1) (VerticalEdge xSign2 x2 y2) =
  xSign1 /= xSign2 && x1 == x2 && Domain1D.includes y1 y2
adjacent (Corner _ ySign1 x1 y1) (HorizontalEdge ySign2 x2 y2) =
  ySign1 /= ySign2 && Domain1D.includes x1 x2 && y1 == y2
adjacent (VerticalEdge xSign1 x1 y1) (Corner xSign2 _ x2 y2) =
  xSign1 /= xSign2 && x1 == x2 && Domain1D.includes y2 y1
adjacent (VerticalEdge xSign1 x1 y1) (VerticalEdge xSign2 x2 y2) =
  xSign1 /= xSign2 && x1 == x2 && Domain1D.overlaps y1 y2
adjacent VerticalEdge{} HorizontalEdge{} =
  False
adjacent (HorizontalEdge ySign1 x1 y1) (Corner _ ySign2 x2 y2) =
  ySign1 /= ySign2 && Domain1D.includes x2 x1 && y1 == y2
adjacent HorizontalEdge{} VerticalEdge{} =
  False
adjacent (HorizontalEdge ySign1 x1 y1) (HorizontalEdge ySign2 x2 y2) =
  ySign1 /= ySign2 && Domain1D.overlaps x1 x2 && y1 == y2

contacts :: Domain2D -> Boundary -> Bool
contacts (Domain2D xDomain yDomain) boundary = case boundary of
  Corner Negative Negative boundaryX boundaryY ->
    (boundaryY == Domain1D.upperBoundary yDomain && Domain1D.includes boundaryX xDomain)
      || (boundaryX == Domain1D.upperBoundary xDomain && Domain1D.includes boundaryY yDomain)
  Corner Positive Negative boundaryX boundaryY ->
    (boundaryY == Domain1D.upperBoundary yDomain && Domain1D.includes boundaryX xDomain)
      || (boundaryX == Domain1D.lowerBoundary xDomain && Domain1D.includes boundaryY yDomain)
  Corner Negative Positive boundaryX boundaryY ->
    (boundaryY == Domain1D.lowerBoundary yDomain && Domain1D.includes boundaryX xDomain)
      || (boundaryX == Domain1D.upperBoundary xDomain && Domain1D.includes boundaryY yDomain)
  Corner Positive Positive boundaryX boundaryY ->
    (boundaryY == Domain1D.lowerBoundary yDomain && Domain1D.includes boundaryX xDomain)
      || (boundaryX == Domain1D.lowerBoundary xDomain && Domain1D.includes boundaryY yDomain)
  VerticalEdge Negative boundaryX boundaryYDomain ->
    boundaryX == Domain1D.upperBoundary xDomain && Domain1D.overlaps yDomain boundaryYDomain
  VerticalEdge Positive boundaryX boundaryYDomain ->
    boundaryX == Domain1D.lowerBoundary xDomain && Domain1D.overlaps yDomain boundaryYDomain
  HorizontalEdge Negative boundaryXDomain boundaryY ->
    boundaryY == Domain1D.upperBoundary yDomain && Domain1D.overlaps xDomain boundaryXDomain
  HorizontalEdge Positive boundaryXDomain boundaryY ->
    boundaryY == Domain1D.lowerBoundary yDomain && Domain1D.overlaps xDomain boundaryXDomain

half :: Domain2D -> Domain2D
half (Domain2D x y) = Domain2D (Domain1D.half x) (Domain1D.half y)

bounds :: Domain2D -> UvBounds
bounds (Domain2D x y) = Bounds2D x.bounds y.bounds

interior :: Domain2D -> UvBounds
interior (Domain2D x y) = Bounds2D (Domain1D.interior x) (Domain1D.interior y)

overlaps :: Domain2D -> Domain2D -> Bool
overlaps (Domain2D x2 y2) (Domain2D x1 y1) = Domain1D.overlaps x2 x1 && Domain1D.overlaps y2 y1

contains :: Domain2D -> Domain2D -> Bool
contains (Domain2D x2 y2) (Domain2D x1 y1) = Domain1D.contains x2 x1 && Domain1D.contains y2 y1

area :: Domain2D -> Number
area (Domain2D x y) = Domain1D.width x * Domain1D.width y

intersectionArea :: Domain2D -> Domain2D -> Number
intersectionArea (Domain2D x1 y1) (Domain2D x2 y2) =
  Domain1D.intersectionWidth x1 x2 * Domain1D.intersectionWidth y1 y2
