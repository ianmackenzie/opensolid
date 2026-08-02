module OpenSolid.Curve.IntersectionPoint
  ( IntersectionPoint (IntersectionPoint)
  , crossing
  , tangent
  , indistinguishable
  , continuity
  , firstParameterValue
  , secondParameterValue
  , parameterValues
  , isJoin
  , isCrossing
  , isTangent
  , isIndistinguishable
  , overlapSign
  )
where

import OpenSolid.Continuity (Continuity)
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude

data IntersectionPoint = IntersectionPoint
  { continuity :: Continuity
  , parameterValues :: (Number, Number)
  }
  deriving (Show)

crossing :: (Number, Number) -> IntersectionPoint
crossing = IntersectionPoint Continuity.Crossing

tangent :: Sign -> (Number, Number) -> IntersectionPoint
tangent alignment = IntersectionPoint (Continuity.Tangent alignment)

indistinguishable :: Sign -> (Number, Number) -> IntersectionPoint
indistinguishable alignment = IntersectionPoint (Continuity.Indistinguishable alignment)

continuity :: IntersectionPoint -> Continuity
continuity = (.continuity)

parameterValues :: IntersectionPoint -> (Number, Number)
parameterValues = (.parameterValues)

firstParameterValue :: IntersectionPoint -> Number
firstParameterValue = Pair.first . parameterValues

secondParameterValue :: IntersectionPoint -> Number
secondParameterValue = Pair.second . parameterValues

isJoin :: IntersectionPoint -> Bool
isJoin intersectionPoint = do
  let (t1, t2) = parameterValues intersectionPoint
  Parameter.isEndpoint t1 && Parameter.isEndpoint t2 && case continuity intersectionPoint of
    Continuity.Crossing -> True
    Continuity.Tangent _ -> True
    Continuity.Indistinguishable Positive -> t1 /= t2
    Continuity.Indistinguishable Negative -> t1 == t2

isCrossing :: IntersectionPoint -> Bool
isCrossing intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Crossing -> True
    _ -> False

isTangent :: IntersectionPoint -> Bool
isTangent intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Tangent _ -> True
    _ -> False

isIndistinguishable :: IntersectionPoint -> Bool
isIndistinguishable intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Indistinguishable _ -> True
    _ -> False

overlapSign :: IntersectionPoint -> Maybe Sign
overlapSign intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Indistinguishable sign -> Just sign
    _ -> Nothing
