module OpenSolid.Region2d
  ( Region2d
  , boundedBy
  , unit
  , outerLoop
  , innerLoops
  , boundaryCurves
  , classify
  , contains
  , bounds
  , area
  , toMesh
  )
where

import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.ConstrainedDelaunayTriangulation qualified as CDT
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import OpenSolid.Curve2d.IntersectionPoint qualified as Curve2d.IntersectionPoint
import OpenSolid.Curve2d.Intersections qualified as Curve2d.Intersections
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.Float qualified as Float
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Polyline2d qualified as Polyline2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Region2d.BoundedBy qualified as BoundedBy
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceParameter (UvCoordinates)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

type role Region2d nominal

data Region2d (coordinateSystem :: CoordinateSystem)
  = Region2d (Loop coordinateSystem) (List (Loop coordinateSystem))

type Loop (coordinateSystem :: CoordinateSystem) =
  NonEmpty (Curve2d coordinateSystem)

instance HasUnits (Region2d (space @ units)) units (Region2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (Region2d (space1 @ unitsA)) (Region2d (space2 @ unitsB))
  where
  coerce (Region2d outer inners) =
    Region2d
      (NonEmpty.map Units.coerce outer)
      (List.map (NonEmpty.map Units.coerce) inners)

boundedBy ::
  Tolerance units =>
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error (Region2d (space @ units))
boundedBy curves = Result.do
  checkForInnerIntersection curves
  loops <- connect curves
  classifyLoops loops

unit :: Region2d UvCoordinates
unit = do
  let p00 = Point2d 0.0 0.0
  let p01 = Point2d 0.0 1.0
  let p10 = Point2d 1.0 0.0
  let p11 = Point2d 1.0 1.0
  let boundaries =
        NonEmpty.four
          (Curve2d.line p00 p10)
          (Curve2d.line p10 p11)
          (Curve2d.line p11 p01)
          (Curve2d.line p01 p00)
  Region2d boundaries []

checkForInnerIntersection ::
  Tolerance units =>
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error ()
checkForInnerIntersection [] = Success ()
checkForInnerIntersection (first : rest) = Result.do
  checkCurveForInnerIntersection first rest
  checkForInnerIntersection rest

checkCurveForInnerIntersection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error ()
checkCurveForInnerIntersection _ [] = Success ()
checkCurveForInnerIntersection curve (first : rest) = Result.do
  checkCurvesForInnerIntersection curve first
  checkCurveForInnerIntersection curve rest

checkCurvesForInnerIntersection ::
  Tolerance units =>
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
  Tolerance units =>
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
  Tolerance units =>
  PartialLoop (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error (Loop (space @ units), List (Curve2d (space @ units)))
buildLoop partialLoop@(PartialLoop currentStart currentCurves loopEnd) remainingCurves
  | currentStart ~= loopEnd = Success (currentCurves, remainingCurves)
  | otherwise = Result.do
      (updatedPartialLoop, updatedRemainingCurves) <- extendPartialLoop partialLoop remainingCurves
      buildLoop updatedPartialLoop updatedRemainingCurves

extendPartialLoop ::
  Tolerance units =>
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
  PartialLoop (Curve2d.startPoint curve) (NonEmpty.one curve) (Curve2d.endPoint curve)

outerLoop :: Region2d (space @ units) -> NonEmpty (Curve2d (space @ units))
outerLoop (Region2d loop _) = loop

innerLoops :: Region2d (space @ units) -> List (NonEmpty (Curve2d (space @ units)))
innerLoops (Region2d _ loops) = loops

boundaryCurves :: Region2d (space @ units) -> NonEmpty (Curve2d (space @ units))
boundaryCurves region = NonEmpty.concat (outerLoop region :| innerLoops region)

contains :: Tolerance units => Point2d (space @ units) -> Region2d (space @ units) -> Bool
contains point region =
  case classify point (boundaryCurves region) of
    Nothing -> True -- Point on boundary is considered contained
    Just Positive -> True
    Just Negative -> False

classify ::
  Tolerance units =>
  Point2d (space @ units) ->
  NonEmpty (Curve2d (space @ units)) ->
  Maybe Sign
classify point curves =
  if NonEmpty.anySatisfy (^ point) curves
    then Nothing
    else Just (classifyNonBoundary point curves)

fluxIntegral ::
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Estimate Unitless
fluxIntegral point curve = do
  let displacement = point - curve
  let firstDerivative = Curve2d.derivative curve
  let integrand = (firstDerivative .><. displacement) / VectorCurve2d.squaredMagnitude' displacement
  Curve.integral integrand

totalFlux ::
  Point2d (space @ units) ->
  Loop (space @ units) ->
  Estimate Unitless
totalFlux point loop =
  Estimate.sum (List.map (fluxIntegral point) (NonEmpty.toList loop))

classifyNonBoundary :: Tolerance units => Point2d (space @ units) -> Loop (space @ units) -> Sign
classifyNonBoundary point loop = do
  let flux = Estimate.satisfy containmentIsDeterminate (totalFlux point loop)
  if Range.includes Qty.zero flux then Negative else Positive

bothPossibleFluxValues :: Range Unitless
bothPossibleFluxValues = Range.from 0.0 Float.twoPi

containmentIsDeterminate :: Range Unitless -> Bool
containmentIsDeterminate flux = not (Range.contains bothPossibleFluxValues flux)

classifyLoops ::
  Tolerance units =>
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

fixSign :: Tolerance units => Sign -> Loop (space @ units) -> Loop (space @ units)
fixSign desiredSign loop =
  Tolerance.using Tolerance.squared' do
    if Estimate.sign (loopSignedArea' loop) == desiredSign then loop else reverseLoop loop

reverseLoop :: Loop (space @ units) -> Loop (space @ units)
reverseLoop loop = NonEmpty.reverseMap Curve2d.reverse loop

pickLargestLoop ::
  Tolerance units =>
  NonEmpty (Loop (space @ units)) ->
  (Loop (space @ units), List (Loop (space @ units)))
pickLargestLoop loops =
  Tolerance.using Tolerance.squared' do
    Estimate.pickLargestBy loopSignedArea' loops

loopSignedArea' :: Loop (space @ units) -> Estimate (units :*: units)
loopSignedArea' loop = do
  let referencePoint = Curve2d.startPoint (NonEmpty.first loop)
  NonEmpty.toList loop
    |> List.map (areaIntegral' referencePoint)
    |> Estimate.sum

areaIntegral ::
  Units.Squared units1 units2 =>
  Point2d (space @ units1) ->
  Curve2d (space @ units1) ->
  Estimate units2
areaIntegral referencePoint curve =
  Units.specialize (areaIntegral' referencePoint curve)

areaIntegral' :: Point2d (space @ units) -> Curve2d (space @ units) -> Estimate (units :*: units)
areaIntegral' referencePoint curve = do
  let displacement = curve - referencePoint
  let y = VectorCurve2d.yComponent displacement
  let dx = Curve.derivative (VectorCurve2d.xComponent displacement)
  -(Curve.integral (y .*. dx))

loopIsInside :: Tolerance units => Loop (space @ units) -> Loop (space @ units) -> Bool
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

area :: Units.Squared units1 units2 => Region2d (space @ units1) -> Estimate units2
area region = do
  let referencePoint = Curve2d.startPoint (NonEmpty.first (outerLoop region))
  Estimate.sum (List.map (areaIntegral referencePoint) (NonEmpty.toList (boundaryCurves region)))

toMesh :: Qty units -> Region2d (space @ units) -> Mesh (Point2d (space @ units))
toMesh accuracy region = do
  let allLoops = outerLoop region :| innerLoops region
  let vertexLoops = NonEmpty.map (toVertexLoop accuracy) allLoops
  CDT.unsafe vertexLoops [] Nothing

toVertexLoop ::
  Qty units ->
  NonEmpty (Curve2d (space @ units)) ->
  NonEmpty (Point2d (space @ units))
toVertexLoop accuracy loop = do
  let curveVertices curve = NonEmpty.rest (Polyline2d.vertices (Curve2d.toPolyline accuracy curve))
  let allVertices = List.collect curveVertices (NonEmpty.toList loop)
  case allVertices of
    NonEmpty vertices -> vertices
    [] -> internalError "Should always have at least one vertex"
