module Curve2d.Intersection
  ( Intersection (..)
  , Kind (..)
  , TangentIntersectionAtDegeneratePoint (TangentIntersectionAtDegeneratePoint)
  )
where

import OpenSolid
import Units (Unitless)

data Intersection = Intersection
  { u1 :: Float
  , u2 :: Float
  , kind :: Kind
  , sign :: Sign
  }
  deriving (Eq, Ord, Show)

data Kind = Crossing | Tangent deriving (Eq, Ord, Show)

data TangentIntersectionAtDegeneratePoint
  = TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, ErrorMessage)

instance ApproximateEquality Intersection Intersection Unitless where
  intersection1 ~= intersection2 =
    intersection1.u1 ~= intersection2.u1
      && intersection1.u2 ~= intersection2.u2
      && intersection1.kind == intersection2.kind
      && intersection1.sign == intersection2.sign
