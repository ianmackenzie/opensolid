module Curve2d.Intersection
  ( Intersection (Intersection, t1, t2, kind, sign)
  , Kind (Crossing, Tangent)
  , TangentIntersectionAtDegeneratePoint (TangentIntersectionAtDegeneratePoint)
  , crossing
  , tangent
  )
where

import OpenSolid

data Intersection = Intersection
  { t1 :: Float
  , t2 :: Float
  , kind :: Kind
  , sign :: Sign
  }
  deriving (Eq, Ord, Show)

data Kind = Crossing | Tangent deriving (Eq, Ord, Show)

data TangentIntersectionAtDegeneratePoint
  = TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, Error)

instance ApproximateEquality Intersection Intersection Unitless where
  intersection1 ~= intersection2 =
    t1 intersection1 ~= t1 intersection2
      && t2 intersection1 ~= t2 intersection2
      && kind intersection1 == kind intersection2
      && sign intersection1 == sign intersection2

crossing :: Float -> Float -> Sign -> Intersection
crossing t1 t2 sign = Intersection {t1, t2, sign, kind = Crossing}

tangent :: Float -> Float -> Sign -> Intersection
tangent t1 t2 sign = Intersection {t1, t2, sign, kind = Tangent}
