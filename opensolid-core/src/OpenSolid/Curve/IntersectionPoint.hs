module OpenSolid.Curve.IntersectionPoint
  ( IntersectionPoint (Crossing, Tangent)
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

import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve qualified as Curve
import OpenSolid.CurvePoint (CurvePoint)
import OpenSolid.CurvePoint qualified as CurvePoint
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point (Point)
import OpenSolid.Prelude

data IntersectionPoint dimension units space
  = Crossing (CurvePoint dimension units space, CurvePoint dimension units space)
  | Tangent Sign (CurvePoint dimension units space, CurvePoint dimension units space)

deriving instance
  Curve.Exists dimension units space =>
  Show (IntersectionPoint dimension units space)

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
curvePoints (Crossing points) = points
curvePoints (Tangent _ points) = points

firstCurvePoint :: IntersectionPoint dimension units space -> CurvePoint dimension units space
firstCurvePoint = Pair.first . curvePoints

secondCurvePoint :: IntersectionPoint dimension units space -> CurvePoint dimension units space
secondCurvePoint = Pair.first . curvePoints

overlapAlignment ::
  (Curve.Exists dimension units space, Tolerance units) =>
  IntersectionPoint dimension units space ->
  Maybe Sign
overlapAlignment (Crossing _) = Nothing
overlapAlignment (Tangent sign (p1, p2))
  | CurvePoint.isDegenerate p1 || CurvePoint.isDegenerate p2 = Just sign
  | CurvePoint.continuity p1 p2 == Just (Continuity.G2 sign) = Just sign
  | otherwise = Nothing
