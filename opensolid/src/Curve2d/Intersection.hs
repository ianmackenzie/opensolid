module Curve2d.Intersection
  ( Intersection (Intersection, u1, u2, kind, sign)
  , Kind (Crossing, Tangent)
  , TangentIntersectionAtDegeneratePoint (TangentIntersectionAtDegeneratePoint)
  , crossing
  , tangent
  )
where

import OpenSolid

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
    u1 intersection1 ~= u1 intersection2
      && u2 intersection1 ~= u2 intersection2
      && kind intersection1 == kind intersection2
      && sign intersection1 == sign intersection2

crossing :: Float -> Float -> Sign -> Intersection
crossing u1 u2 sign = Intersection {u1, u2, sign, kind = Crossing}

tangent :: Float -> Float -> Sign -> Intersection
tangent u1 u2 sign = Intersection {u1, u2, sign, kind = Tangent}
