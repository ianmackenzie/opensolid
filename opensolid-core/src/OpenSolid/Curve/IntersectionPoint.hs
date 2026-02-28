module OpenSolid.Curve.IntersectionPoint
  ( Kind (Crossing, Tangent)
  , IntersectionPoint
  , parameterValues
  , kind
  , new
  , crossing
  , tangent
  )
where

import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

data Kind = Crossing | Tangent Sign deriving (Eq, Ord, Show)

data IntersectionPoint = IntersectionPoint
  { parameterValues :: (Number, Number)
  , kind :: Kind
  }
  deriving (Eq, Ord, Show)

instance ApproximateEquality IntersectionPoint () where
  point1 ~= point2 =
    Tolerance.using Tolerance.unitless (point1.parameterValues ~= point2.parameterValues)
      && point1.kind == point2.kind

parameterValues :: IntersectionPoint -> (Number, Number)
parameterValues = (.parameterValues)

kind :: IntersectionPoint -> Kind
kind = (.kind)

new :: Kind -> Number -> Number -> IntersectionPoint
new givenKind t1 t2 = IntersectionPoint{parameterValues = (t1, t2), kind = givenKind}

crossing :: Number -> Number -> IntersectionPoint
crossing t1 t2 = IntersectionPoint{parameterValues = (t1, t2), kind = Crossing}

tangent :: Number -> Number -> Sign -> IntersectionPoint
tangent t1 t2 sign = IntersectionPoint{parameterValues = (t1, t2), kind = Tangent sign}
