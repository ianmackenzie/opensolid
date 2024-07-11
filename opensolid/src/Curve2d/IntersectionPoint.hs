module Curve2d.IntersectionPoint
  ( IntersectionPoint (IntersectionPoint, t1, t2, kind, sign)
  , Kind (Crossing, Tangent)
  , crossing
  , tangent
  )
where

import OpenSolid

data IntersectionPoint = IntersectionPoint
  { t1 :: Float
  , t2 :: Float
  , kind :: Kind
  , sign :: Sign
  }
  deriving (Eq, Ord, Show)

data Kind = Crossing | Tangent deriving (Eq, Ord, Show)

instance ApproximateEquality IntersectionPoint IntersectionPoint Unitless where
  intersection1 ~= intersection2 =
    t1 intersection1 ~= t1 intersection2
      && t2 intersection1 ~= t2 intersection2
      && kind intersection1 == kind intersection2
      && sign intersection1 == sign intersection2

crossing :: Float -> Float -> Sign -> IntersectionPoint
crossing t1 t2 sign = IntersectionPoint{t1, t2, sign, kind = Crossing}

tangent :: Float -> Float -> Sign -> IntersectionPoint
tangent t1 t2 sign = IntersectionPoint{t1, t2, sign, kind = Tangent}
