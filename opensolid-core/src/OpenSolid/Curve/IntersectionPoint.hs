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
  , overlapAlignment
  )
where

import OpenSolid.Continuity (Continuity)
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve qualified as Curve
import OpenSolid.CurvePoint (CurvePoint)
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.Pair qualified as Pair
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

overlapAlignment ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Maybe Sign
overlapAlignment (IntersectionPoint Continuity.G0 _) = Nothing
overlapAlignment (IntersectionPoint (Continuity.G2 sign) _) = Just sign
overlapAlignment (IntersectionPoint (Continuity.G1 sign) (p1, p2)) =
  if CurvePoint.isDegenerate p1 || CurvePoint.isDegenerate p2 then Just sign else Nothing
