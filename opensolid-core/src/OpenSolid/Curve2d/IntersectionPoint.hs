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
  = Crossing Float Float Sign
  | Tangent Float Float Sign
  | Corner Float Float
  deriving (Eq, Ord, Show)

instance ApproximateEquality IntersectionPoint IntersectionPoint Unitless where
  Crossing u1 v1 sign1 ~= Crossing u2 v2 sign2 = u1 ~= u2 && v1 ~= v2 && sign1 == sign2
  Crossing{} ~= _ = False
  Tangent u1 v1 sign1 ~= Tangent u2 v2 sign2 = u1 ~= u2 && v1 ~= v2 && sign1 == sign2
  Tangent{} ~= _ = False
  Corner u1 v1 ~= Corner u2 v2 = u1 ~= u2 && v1 ~= v2
  Corner{} ~= _ = False

crossing :: Float -> Float -> Sign -> IntersectionPoint
crossing = Crossing

tangent :: Float -> Float -> Sign -> IntersectionPoint
tangent = Tangent

corner :: Float -> Float -> IntersectionPoint
corner = Corner

parameterValues :: IntersectionPoint -> (Float, Float)
parameterValues (Crossing t1 t2 _) = (t1, t2)
parameterValues (Tangent t1 t2 _) = (t1, t2)
parameterValues (Corner t1 t2) = (t1, t2)
