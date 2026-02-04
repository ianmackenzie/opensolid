module OpenSolid.Curve.IntersectionPoint
  ( Kind (Crossing, Tangent)
  , IntersectionPoint (IntersectionPoint, kind, t1, t2)
  , crossing
  , tangent
  )
where

import OpenSolid.Prelude

data Kind = Crossing | Tangent deriving (Eq, Ord, Show)

data IntersectionPoint = IntersectionPoint
  { t1 :: Number
  , t2 :: Number
  , kind :: Kind
  }
  deriving (Eq, Ord, Show)

crossing :: Number -> Number -> IntersectionPoint
crossing t1 t2 = IntersectionPoint{t1, t2, kind = Crossing}

tangent :: Number -> Number -> IntersectionPoint
tangent t1 t2 = IntersectionPoint{t1, t2, kind = Tangent}
