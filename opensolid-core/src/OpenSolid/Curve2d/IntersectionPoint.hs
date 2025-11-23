module OpenSolid.Curve2d.IntersectionPoint
  ( Kind (Crossing, Tangent)
  , IntersectionPoint (IntersectionPoint, kind, t1, t2)
  , crossing
  , tangent
  )
where

import OpenSolid.Prelude

data Kind = Crossing Sign | Tangent deriving (Eq, Ord, Show)

data IntersectionPoint = IntersectionPoint
  { t1 :: Number
  , t2 :: Number
  , kind :: Kind
  }
  deriving (Eq, Ord, Show)

instance ApproximateEquality IntersectionPoint Unitless where
  first ~= second = first.kind == second.kind && first.t1 ~= second.t1 && first.t2 ~= second.t2

crossing :: Number -> Number -> Sign -> IntersectionPoint
crossing t1 t2 sign = IntersectionPoint{t1, t2, kind = Crossing sign}

tangent :: Number -> Number -> IntersectionPoint
tangent t1 t2 = IntersectionPoint{t1, t2, kind = Tangent}
