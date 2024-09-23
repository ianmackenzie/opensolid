module Region2d
  ( Region2d
  , boundedBy
  , outerLoop
  , innerLoops
  , boundaryCurves
  , classify
  , contains
  , bounds
  , area
  , toPolygon
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Curve1d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import Curve2d.IntersectionPoint qualified
import Curve2d.Intersections qualified
import Estimate (Estimate)
import Estimate qualified
import Float qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Polygon2d (Polygon2d)
import Polygon2d qualified
import Polyline2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import Region2d.BoundedBy qualified as BoundedBy
import Result qualified
import Tolerance qualified
import Units qualified
import VectorCurve2d qualified
import Vertex2d (Vertex2d)

type role Region2d nominal

data Region2d (coordinateSystem :: CoordinateSystem)
  = Region2d (Loop coordinateSystem) (List (Loop coordinateSystem))

type Loop (coordinateSystem :: CoordinateSystem) =
  NonEmpty (Curve2d coordinateSystem)

boundedBy ::
  (Known space, Known units, Tolerance units) =>
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error (Region2d (space @ units))
boundedBy curves = Result.do
  checkForInnerIntersection curves
  loops <- connect curves
  classifyLoops loops

checkForInnerIntersection ::
  (Known space, Known units, Tolerance units) =>
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error ()
checkForInnerIntersection [] = Success ()
checkForInnerIntersection (first : rest) = Result.do
  checkCurveForInnerIntersection first rest
  checkForInnerIntersection rest

checkCurveForInnerIntersection ::
  (Known space, Known units, Tolerance units) =>
  Curve2d (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error ()
checkCurveForInnerIntersection _ [] = Success ()
checkCurveForInnerIntersection curve (first : rest) = Result.do
  checkCurvesForInnerIntersection curve first
  checkCurveForInnerIntersection curve rest

checkCurvesForInnerIntersection ::
  (Known space, Known units, Tolerance units) =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result BoundedBy.Error ()
checkCurvesForInnerIntersection curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Failure Curve2d.Intersections.CurveHasDegeneracy ->
      Failure BoundedBy.BoundaryCurveHasDegeneracy
    Failure Curve2d.Intersections.HigherOrderIntersection ->
      Failure BoundedBy.BoundaryCurvesHaveHigherOrderIntersection
    -- We can ignore cases where either curve is actually a point,
    -- since we'll still find any inner intersections
    -- when we check with the *neighbours* of those degenerate curves
    Failure Curve2d.Intersections.FirstCurveIsPoint -> Success ()
    Failure Curve2d.Intersections.SecondCurveIsPoint -> Success ()
    -- Any overlap between boundary curves is bad
    Success (Just (Curve2d.OverlappingSegments _)) ->
      Failure BoundedBy.BoundaryIntersectsItself
    -- If there are no intersections at all then we're good!
    Success Nothing -> Success ()
    -- Otherwise, make sure curves only intersect (meet) at endpoints
    Success (Just (Curve2d.IntersectionPoints intersectionPoints)) ->
      if NonEmpty.allSatisfy isEndpointIntersection intersectionPoints
        then Success ()
        else Failure BoundedBy.BoundaryIntersectsItself

isEndpointIntersection :: IntersectionPoint -> Bool
isEndpointIntersection (IntersectionPoint{t1, t2}) = isEndpoint t1 && isEndpoint t2

isEndpoint :: Float -> Bool
isEndpoint t = t == 0.0 || t == 1.0

connect ::
  (Known space, Known units, Tolerance units) =>
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error (List (Loop (space @ units)))
connect [] = Success []
connect (first : rest) = Result.do
  (loop, remainingCurves) <- buildLoop (startLoop first) rest
  remainingLoops <- connect remainingCurves
  Success (loop : remainingLoops)

data PartialLoop coordinateSystem
  = PartialLoop
      (Point2d coordinateSystem)
      (NonEmpty (Curve2d coordinateSystem))
      (Point2d coordinateSystem)

buildLoop ::
  (Known space, Known units, Tolerance units) =>
  PartialLoop (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error (Loop (space @ units), List (Curve2d (space @ units)))
buildLoop partialLoop@(PartialLoop currentStart currentCurves loopEnd) remainingCurves
  | currentStart ~= loopEnd = Success (currentCurves, remainingCurves)
  | otherwise = Result.do
      (updatedPartialLoop, updatedRemainingCurves) <- extendPartialLoop partialLoop remainingCurves
      buildLoop updatedPartialLoop updatedRemainingCurves

extendPartialLoop ::
  (Known space, Known units, Tolerance units) =>
  PartialLoop (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error (PartialLoop (space @ units), List (Curve2d (space @ units)))
extendPartialLoop (PartialLoop currentStart currentCurves loopEnd) curves =
  case List.partition (hasEndpoint currentStart) curves of
    ([], _) -> Failure BoundedBy.BoundaryHasGaps
    (List.One curve, remaining) -> do
      let newCurve = if Curve2d.endPoint curve ~= currentStart then curve else Curve2d.reverse curve
      let newStart = Curve2d.startPoint newCurve
      let updatedCurves = NonEmpty.prepend newCurve currentCurves
      Success (PartialLoop newStart updatedCurves loopEnd, remaining)
    (List.TwoOrMore, _) -> Failure BoundedBy.BoundaryIntersectsItself

hasEndpoint :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Bool
hasEndpoint point curve =
  Curve2d.startPoint curve ~= point || Curve2d.endPoint curve ~= point

startLoop :: Curve2d (space @ units) -> PartialLoop (space @ units)
startLoop curve =
  PartialLoop (Curve2d.startPoint curve) (NonEmpty.singleton curve) (Curve2d.endPoint curve)

outerLoop :: Region2d (space @ units) -> NonEmpty (Curve2d (space @ units))
outerLoop (Region2d loop _) = loop

innerLoops :: Region2d (space @ units) -> List (NonEmpty (Curve2d (space @ units)))
innerLoops (Region2d _ loops) = loops

boundaryCurves :: Region2d (space @ units) -> NonEmpty (Curve2d (space @ units))
boundaryCurves region = NonEmpty.concat (outerLoop region :| innerLoops region)

contains ::
  (Known space, Known units, Tolerance units) =>
  Point2d (space @ units) ->
  Region2d (space @ units) ->
  Bool
contains point region =
  case classify point (boundaryCurves region) of
    Nothing -> True -- Point on boundary is considered contained
    Just Positive -> True
    Just Negative -> False

classify ::
  (Known space, Known units, Tolerance units) =>
  Point2d (space @ units) ->
  NonEmpty (Curve2d (space @ units)) ->
  Maybe Sign
classify point curves =
  if NonEmpty.anySatisfy (^ point) curves
    then Nothing
    else Just (classifyNonBoundary point curves)

fluxIntegral ::
  (Known space, Known units) =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Estimate Unitless
fluxIntegral point curve = do
  let displacement = point - curve
  let firstDerivative = Curve2d.derivative curve
  let integrand = (firstDerivative .><. displacement) / VectorCurve2d.squaredMagnitude' displacement
  Curve1d.integral integrand

totalFlux ::
  (Known space, Known units) =>
  Point2d (space @ units) ->
  Loop (space @ units) ->
  Estimate Unitless
totalFlux point loop =
  Estimate.sum (List.map (fluxIntegral point) (NonEmpty.toList loop))

classifyNonBoundary ::
  (Known space, Known units, Tolerance units) =>
  Point2d (space @ units) ->
  Loop (space @ units) ->
  Sign
classifyNonBoundary point loop = do
  let flux = Estimate.satisfy containmentIsDeterminate (totalFlux point loop)
  if Range.includes Qty.zero flux then Negative else Positive

bothPossibleFluxValues :: Range Unitless
bothPossibleFluxValues = Range.from 0.0 Float.twoPi

containmentIsDeterminate :: Range Unitless -> Bool
containmentIsDeterminate flux = not (Range.contains bothPossibleFluxValues flux)

classifyLoops ::
  (Known space, Known units, Tolerance units) =>
  List (Loop (space @ units)) ->
  Result BoundedBy.Error (Region2d (space @ units))
classifyLoops [] = Failure BoundedBy.EmptyRegion
classifyLoops (NonEmpty loops) = Result.do
  let (largestLoop, smallerLoops) = pickLargestLoop loops
  let outerLoopCandidate = fixSign Positive largestLoop
  let innerLoopCandidates = List.map (fixSign Negative) smallerLoops
  if List.allSatisfy (loopIsInside outerLoopCandidate) innerLoopCandidates
    then Success (Region2d outerLoopCandidate innerLoopCandidates)
    else Failure BoundedBy.MultipleDisjointRegions

fixSign ::
  (Known space, Known units, Tolerance units) =>
  Sign ->
  Loop (space @ units) ->
  Loop (space @ units)
fixSign desiredSign loop =
  Tolerance.using Tolerance.squared' $
    if Estimate.sign (loopSignedArea' loop) == desiredSign then loop else reverseLoop loop

reverseLoop :: (Known space, Known units) => Loop (space @ units) -> Loop (space @ units)
reverseLoop loop = NonEmpty.reverseMap Curve2d.reverse loop

pickLargestLoop ::
  (Known space, Known units, Tolerance units) =>
  NonEmpty (Loop (space @ units)) ->
  (Loop (space @ units), List (Loop (space @ units)))
pickLargestLoop loops =
  Tolerance.using Tolerance.squared' $
    Estimate.pickLargestBy loopSignedArea' loops

loopSignedArea' :: (Known space, Known units) => Loop (space @ units) -> Estimate (units :*: units)
loopSignedArea' loop = do
  let referencePoint = Curve2d.startPoint (NonEmpty.first loop)
  NonEmpty.toList loop
    |> List.map (areaIntegral' referencePoint)
    |> Estimate.sum

areaIntegral ::
  (Known space, Known units1, Known units2, Units.Squared units1 units2) =>
  Point2d (space @ units1) ->
  Curve2d (space @ units1) ->
  Estimate units2
areaIntegral referencePoint curve =
  Units.specialize (areaIntegral' referencePoint curve)

areaIntegral' ::
  (Known space, Known units) =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Estimate (units :*: units)
areaIntegral' referencePoint curve = do
  let displacement = curve - referencePoint
  let y = VectorCurve2d.yComponent displacement
  let dx = Curve1d.derivative (VectorCurve2d.xComponent displacement)
  -(Curve1d.integral (y .*. dx))

loopIsInside ::
  (Known space, Known units, Tolerance units) =>
  Loop (space @ units) ->
  Loop (space @ units) ->
  Bool
loopIsInside outer inner = do
  let testPoint = Curve2d.startPoint (NonEmpty.first inner)
  case classify testPoint outer of
    Nothing -> True -- Shouldn't happen, loops should be guaranteed not to be touching by this point
    Just Positive -> True
    Just Negative -> False

bounds :: Region2d (space @ units) -> Bounds2d (space @ units)
bounds region =
  NonEmpty.reduce Bounds2d.aggregate2 $
    NonEmpty.map Curve2d.bounds (outerLoop region)

area ::
  (Known space, Known units1, Known units2, Units.Squared units1 units2) =>
  Region2d (space @ units1) ->
  Estimate units2
area region = do
  let referencePoint = Curve2d.startPoint (NonEmpty.first (outerLoop region))
  Estimate.sum (List.map (areaIntegral referencePoint) (NonEmpty.toList (boundaryCurves region)))

toPolygon ::
  (Known space, Known units, Vertex2d vertex (space @ units)) =>
  Qty units ->
  (Curve2d (space @ units) -> Float -> vertex) ->
  Region2d (space @ units) ->
  Polygon2d vertex
toPolygon maxError function region =
  Polygon2d.withHoles
    (List.map (toPolygonLoop maxError function) (innerLoops region))
    (toPolygonLoop maxError function (outerLoop region))

toPolygonLoop ::
  (Known space, Known units) =>
  Qty units ->
  (Curve2d (space @ units) -> Float -> vertex) ->
  NonEmpty (Curve2d (space @ units)) ->
  NonEmpty vertex
toPolygonLoop maxError function loop = do
  let curveVertices curve =
        Curve2d.toPolyline maxError (function curve) curve
          |> Polyline2d.vertices
          |> NonEmpty.rest
  let allVertices = List.collect curveVertices (NonEmpty.toList loop)
  case allVertices of
    NonEmpty vertices -> vertices
    [] -> internalError "Should always have at least one vertex"
