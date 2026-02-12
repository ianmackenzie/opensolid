module OpenSolid.Curve.IntersectionPoint
  ( Kind (Crossing, Tangent)
  , IntersectionPoint
  , parameterValues
  , kind
  , crossing
  , tangent
  )
where

import OpenSolid.Prelude

data Kind = Crossing | Tangent deriving (Eq, Ord, Show)

data IntersectionPoint = IntersectionPoint
  { parameterValues :: (Number, Number)
  , kind :: Kind
  }
  deriving (Eq, Ord, Show)

parameterValues :: IntersectionPoint -> (Number, Number)
parameterValues = (.parameterValues)

kind :: IntersectionPoint -> Kind
kind = (.kind)

crossing :: Number -> Number -> IntersectionPoint
crossing t1 t2 = IntersectionPoint{parameterValues = (t1, t2), kind = Crossing}

tangent :: Number -> Number -> IntersectionPoint
tangent t1 t2 = IntersectionPoint{parameterValues = (t1, t2), kind = Tangent}
