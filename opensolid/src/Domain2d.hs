module Domain2d
  ( Domain2d (Domain2d)
  , Boundary
  , unit
  , isAtomic
  , coordinates
  , leftBoundary
  , rightBoundary
  , bottomBoundary
  , topBoundary
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

import Bounds2d qualified
import Domain1d (Domain1d)
import Domain1d qualified
import OpenSolid
import Uv qualified

data Domain2d = Domain2d Domain1d Domain1d deriving (Show)

data Boundary
  = VerticalBoundary Domain1d.Boundary Domain1d
  | HorizontalBoundary Domain1d Domain1d.Boundary
  deriving (Show)

unit :: Domain2d
unit = Domain2d Domain1d.unit Domain1d.unit

isAtomic :: Domain2d -> Bool
isAtomic (Domain2d x y) = Domain1d.isAtomic x && Domain1d.isAtomic y

coordinates :: Domain2d -> (Domain1d, Domain1d)
coordinates (Domain2d x y) = (x, y)

leftBoundary :: Domain2d -> Boundary
leftBoundary (Domain2d x y) = VerticalBoundary (Domain1d.lowerBoundary x) y

rightBoundary :: Domain2d -> Boundary
rightBoundary (Domain2d x y) = VerticalBoundary (Domain1d.upperBoundary x) y

bottomBoundary :: Domain2d -> Boundary
bottomBoundary (Domain2d x y) = HorizontalBoundary x (Domain1d.lowerBoundary y)

topBoundary :: Domain2d -> Boundary
topBoundary (Domain2d x y) = HorizontalBoundary x (Domain1d.upperBoundary y)

adjacent :: Boundary -> Boundary -> Bool
adjacent (HorizontalBoundary{}) (VerticalBoundary{}) = False
adjacent (VerticalBoundary{}) (HorizontalBoundary{}) = False
adjacent (HorizontalBoundary x1 y1) (HorizontalBoundary x2 y2) = Domain1d.overlaps x1 x2 && y1 == y2
adjacent (VerticalBoundary x1 y1) (VerticalBoundary x2 y2) = x1 == x2 && Domain1d.overlaps y1 y2

contacts :: Domain2d -> Boundary -> Bool
contacts (Domain2d x y) boundary = case boundary of
  VerticalBoundary bx by ->
    Domain1d.overlaps by y && (bx == Domain1d.lowerBoundary x || bx == Domain1d.upperBoundary x)
  HorizontalBoundary bx by ->
    Domain1d.overlaps bx x && (by == Domain1d.lowerBoundary y || by == Domain1d.upperBoundary y)

half :: Domain2d -> Domain2d
half (Domain2d x y) = Domain2d (Domain1d.half x) (Domain1d.half y)

bounds :: Domain2d -> Uv.Bounds
bounds (Domain2d x y) = Bounds2d.xy (Domain1d.bounds x) (Domain1d.bounds y)

interior :: Domain2d -> Uv.Bounds
interior (Domain2d x y) = Bounds2d.xy (Domain1d.interior x) (Domain1d.interior y)

overlaps :: Domain2d -> Domain2d -> Bool
overlaps (Domain2d x2 y2) (Domain2d x1 y1) = Domain1d.overlaps x2 x1 && Domain1d.overlaps y2 y1

contains :: Domain2d -> Domain2d -> Bool
contains (Domain2d x2 y2) (Domain2d x1 y1) = Domain1d.contains x2 x1 && Domain1d.contains y2 y1

area :: Domain2d -> Float
area (Domain2d x y) = Domain1d.width x * Domain1d.width y

intersectionArea :: Domain2d -> Domain2d -> Float
intersectionArea (Domain2d x1 y1) (Domain2d x2 y2) =
  Domain1d.intersectionWidth x1 x2 * Domain1d.intersectionWidth y1 y2
