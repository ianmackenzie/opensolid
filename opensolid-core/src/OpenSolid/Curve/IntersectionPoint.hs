module OpenSolid.Curve.IntersectionPoint
  ( Kind (Crossing, Tangent)
  , IntersectionPoint (IntersectionPoint)
  , kind
  , firstParameterValue
  , secondParameterValue
  , parameterValues
  , crossing
  , tangent
  )
where

import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

data Kind = Crossing | Tangent Sign deriving (Eq, Ord, Show)

data IntersectionPoint = IntersectionPoint Kind Number Number
  deriving (Eq, Ord, Show)

instance ApproximateEquality IntersectionPoint () where
  IntersectionPoint kind1 u1 v1 ~= IntersectionPoint kind2 u2 v2 =
    kind1 == kind2 && Tolerance.using Tolerance.unitless (u1 ~= u2 && v1 ~= v2)

{-# INLINE kind #-}
kind :: IntersectionPoint -> Kind
kind (IntersectionPoint k _ _) = k

{-# INLINE firstParameterValue #-}
firstParameterValue :: IntersectionPoint -> Number
firstParameterValue (IntersectionPoint _ t1 _) = t1

{-# INLINE secondParameterValue #-}
secondParameterValue :: IntersectionPoint -> Number
secondParameterValue (IntersectionPoint _ _ t2) = t2

{-# INLINE parameterValues #-}
parameterValues :: IntersectionPoint -> (Number, Number)
parameterValues (IntersectionPoint _ t1 t2) = (t1, t2)

crossing :: Number -> Number -> IntersectionPoint
crossing = IntersectionPoint Crossing

tangent :: Sign -> Number -> Number -> IntersectionPoint
tangent sign = IntersectionPoint (Tangent sign)
