module OpenSolid.Curve2d.IntersectionPoint
  ( IntersectionPoint (Crossing, Tangent, Corner)
  , crossing
  , tangent
  , corner
  , parameterValues
  )
where

import OpenSolid.Prelude

data IntersectionPoint
  = Crossing Number Number Sign
  | Tangent Number Number Sign
  | Corner Number Number
  deriving (Eq, Ord, Show)

instance ApproximateEquality IntersectionPoint Unitless where
  Crossing u1 v1 sign1 ~= Crossing u2 v2 sign2 = u1 ~= u2 && v1 ~= v2 && sign1 == sign2
  Crossing{} ~= _ = False
  Tangent u1 v1 sign1 ~= Tangent u2 v2 sign2 = u1 ~= u2 && v1 ~= v2 && sign1 == sign2
  Tangent{} ~= _ = False
  Corner u1 v1 ~= Corner u2 v2 = u1 ~= u2 && v1 ~= v2
  Corner{} ~= _ = False

crossing :: Number -> Number -> Sign -> IntersectionPoint
crossing = Crossing

tangent :: Number -> Number -> Sign -> IntersectionPoint
tangent = Tangent

corner :: Number -> Number -> IntersectionPoint
corner = Corner

parameterValues :: IntersectionPoint -> (Number, Number)
parameterValues (Crossing t1 t2 _) = (t1, t2)
parameterValues (Tangent t1 t2 _) = (t1, t2)
parameterValues (Corner t1 t2) = (t1, t2)
