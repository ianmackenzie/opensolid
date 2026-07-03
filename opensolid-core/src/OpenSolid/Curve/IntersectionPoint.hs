module OpenSolid.Curve.IntersectionPoint
  ( IntersectionPoint (IntersectionPoint)
  , g0
  , g1
  , g2
  , continuity
  , firstParameterValue
  , secondParameterValue
  , parameterValues
  , point
  , curvePoints
  , firstCurvePoint
  , secondCurvePoint
  , Classification (..)
  , classify
  , isJoin
  , isCrossing
  , isTangent
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

g0 ::
  (CurvePoint dimension units space, CurvePoint dimension units space) ->
  IntersectionPoint dimension units space
g0 = IntersectionPoint Continuity.G0

g1 ::
  Sign ->
  (CurvePoint dimension units space, CurvePoint dimension units space) ->
  IntersectionPoint dimension units space
g1 alignment = IntersectionPoint (Continuity.G1 alignment)

g2 ::
  Sign ->
  (CurvePoint dimension units space, CurvePoint dimension units space) ->
  IntersectionPoint dimension units space
g2 alignment = IntersectionPoint (Continuity.G2 alignment)

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

data Classification
  = Join
  | Crossing
  | Tangent
  | Overlap Sign
  deriving (Eq, Show)

classify ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Classification
classify intersectionPoint = do
  let (p1, p2) = curvePoints intersectionPoint
  let (t1, t2) = parameterValues intersectionPoint
  let joinOrPositiveOverlap =
        if (t1 == 0.0 && t2 == 1.0) || (t1 == 1.0 && t2 == 0.0) then Join else Overlap Positive
  let joinOrNegativeOverlap =
        if (t1 == 0.0 && t2 == 0.0) || (t1 == 1.0 && t2 == 1.0) then Join else Overlap Negative
  let hasDegeneracy = CurvePoint.isDegenerate p1 || CurvePoint.isDegenerate p2
  case continuity intersectionPoint of
    Continuity.G0 -> if Parameter.isEndpoint t1 && Parameter.isEndpoint t2 then Join else Crossing
    Continuity.G1 Positive -> if hasDegeneracy then joinOrPositiveOverlap else Tangent
    Continuity.G1 Negative -> if hasDegeneracy then joinOrNegativeOverlap else Tangent
    Continuity.G2 Positive -> joinOrPositiveOverlap
    Continuity.G2 Negative -> joinOrNegativeOverlap

isJoin ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Bool
isJoin intersectionPoint = classify intersectionPoint == Join

isCrossing ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Bool
isCrossing intersectionPoint = classify intersectionPoint == Crossing

isTangent ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Bool
isTangent intersectionPoint = classify intersectionPoint == Tangent

overlapSign ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Maybe Sign
overlapSign intersectionPoint =
  case classify intersectionPoint of
    Overlap sign -> Just sign
    _ -> Nothing
