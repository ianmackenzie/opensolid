module OpenSolid.Curve2D.EndpointIntersection
  ( EndpointIntersection (EndpointIntersection, intersectionPoint, isSingular, alignment)
  , find
  , isLocal
  )
where

import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve2D.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve2D.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.DirectionCurve2D (DirectionCurve2D)
import OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

data EndpointIntersection = EndpointIntersection
  { intersectionPoint :: IntersectionPoint
  , isSingular :: Bool
  , alignment :: Sign
  }
  deriving (Show)

find ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  DirectionCurve2D space ->
  DirectionCurve2D space ->
  Result Curve2D.IsPoint (List EndpointIntersection)
find curve1 curve2 tangent1 tangent2 = do
  start1t2s <- Curve2D.findPoint (Curve2D.startPoint curve1) curve2
  end1t2s <- Curve2D.findPoint (Curve2D.endPoint curve1) curve2
  start2t1s <- Curve2D.findPoint (Curve2D.startPoint curve2) curve1
  end2t1s <- Curve2D.findPoint (Curve2D.endPoint curve2) curve1
  let start1Solutions = List.map (0,) start1t2s
  let end1Solutions = List.map (1,) end1t2s
  let start2Solutions = List.map (,0) start2t1s
  let end2Solutions = List.map (,1) end2t1s
  let allSolutions = List.concat [start1Solutions, end1Solutions, start2Solutions, end2Solutions]
  let uniqueSolutions = List.sortAndDeduplicate allSolutions
  Ok (List.map (toEndpointIntersection curve1 curve2 tangent1 tangent2) uniqueSolutions)

toEndpointIntersection ::
  Tolerance units =>
  Curve2D units space ->
  Curve2D units space ->
  DirectionCurve2D space ->
  DirectionCurve2D space ->
  (Number, Number) ->
  EndpointIntersection
toEndpointIntersection curve1 curve2 tangent1 tangent2 (t1, t2) = do
  let tangentDirection1 = DirectionCurve2D.evaluate tangent1 t1
  let tangentDirection2 = DirectionCurve2D.evaluate tangent2 t2
  let crossProduct = tangentDirection1 `cross` tangentDirection2
  let singular1 = VectorCurve2D.evaluate (Curve2D.derivative curve1) t1 ~= Vector2D.zero
  let singular2 = VectorCurve2D.evaluate (Curve2D.derivative curve2) t2 ~= Vector2D.zero
  let intersectionPoint =
        if Tolerance.using 1e-9 (crossProduct ~= Quantity.zero)
          then IntersectionPoint.tangent t1 t2
          else IntersectionPoint.crossing t1 t2 (Quantity.sign crossProduct)
  let isSingular = singular1 || singular2
  let alignment = Quantity.sign (tangentDirection1 `dot` tangentDirection2)
  EndpointIntersection{intersectionPoint, isSingular, alignment}

isLocal :: Interval Unitless -> Interval Unitless -> EndpointIntersection -> Bool
isLocal tBounds1 tBounds2 EndpointIntersection{intersectionPoint} =
  Interval.includes intersectionPoint.t1 tBounds1 && Interval.includes intersectionPoint.t2 tBounds2
