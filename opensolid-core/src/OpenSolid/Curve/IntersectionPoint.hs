module OpenSolid.Curve.IntersectionPoint
  ( IntersectionPoint (IntersectionPoint)
  , crossing
  , tangent
  , indistinguishable
  , continuity
  , firstParameterValue
  , secondParameterValue
  , parameterValues
  , point
  , curvePoints
  , firstCurvePoint
  , secondCurvePoint
  , isJoin
  , isCrossing
  , isTangent
  , isIndistinguishable
  , overlapSign
  )
where

import OpenSolid.Continuity (Continuity)
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve qualified as Curve
import OpenSolid.CurvePoint (CurvePoint)
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point (Point)
import OpenSolid.Prelude

data IntersectionPoint dimension units space = IntersectionPoint
  { continuity :: Continuity
  , curvePoints :: (CurvePoint dimension units space, CurvePoint dimension units space)
  }

deriving instance
  Curve.Exists dimension units space =>
  Show (IntersectionPoint dimension units space)

crossing ::
  (CurvePoint dimension units space, CurvePoint dimension units space) ->
  IntersectionPoint dimension units space
crossing = IntersectionPoint Continuity.Crossing

tangent ::
  Sign ->
  (CurvePoint dimension units space, CurvePoint dimension units space) ->
  IntersectionPoint dimension units space
tangent alignment = IntersectionPoint (Continuity.Tangent alignment)

indistinguishable ::
  Sign ->
  (CurvePoint dimension units space, CurvePoint dimension units space) ->
  IntersectionPoint dimension units space
indistinguishable alignment = IntersectionPoint (Continuity.Indistinguishable alignment)

continuity :: IntersectionPoint dimension units space -> Continuity
continuity = (.continuity)

parameterValues :: IntersectionPoint dimension units space -> (Number, Number)
parameterValues = Pair.map CurvePoint.parameterValue . curvePoints

firstParameterValue :: IntersectionPoint dimension units space -> Number
firstParameterValue = CurvePoint.parameterValue . firstCurvePoint

secondParameterValue :: IntersectionPoint dimension units space -> Number
secondParameterValue = CurvePoint.parameterValue . secondCurvePoint

point :: IntersectionPoint dimension units space -> Point dimension units space
point = CurvePoint.point . firstCurvePoint

curvePoints ::
  IntersectionPoint dimension units space ->
  (CurvePoint dimension units space, CurvePoint dimension units space)
curvePoints = (.curvePoints)

firstCurvePoint :: IntersectionPoint dimension units space -> CurvePoint dimension units space
firstCurvePoint = Pair.first . curvePoints

secondCurvePoint :: IntersectionPoint dimension units space -> CurvePoint dimension units space
secondCurvePoint = Pair.first . curvePoints

isJoin ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Bool
isJoin intersectionPoint = do
  let (t1, t2) = parameterValues intersectionPoint
  Parameter.isEndpoint t1 && Parameter.isEndpoint t2 && case continuity intersectionPoint of
    Continuity.Crossing -> True
    Continuity.Tangent _ -> True
    Continuity.Indistinguishable Positive -> t1 /= t2
    Continuity.Indistinguishable Negative -> t1 == t2

isCrossing ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Bool
isCrossing intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Crossing -> True
    _ -> False

isTangent ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Bool
isTangent intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Tangent _ -> True
    _ -> False

isIndistinguishable ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Bool
isIndistinguishable intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Indistinguishable _ -> True
    _ -> False

overlapSign ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Maybe Sign
overlapSign intersectionPoint =
  case continuity intersectionPoint of
    Continuity.Indistinguishable sign -> Just sign
    _ -> Nothing
