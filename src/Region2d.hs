module Region2d
  ( Region2d
  , BuildError (..)
  , boundedBy
  , outerLoop
  , innerLoops
  , boundaryCurves
  , classify
  , contains
  , boundingBox
  , area
  )
where

import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.Intersection (Intersection (Intersection))
import Estimate (Estimate)
import Estimate qualified
import Float qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Qty qualified
import Range (Range)
import Range qualified
import Units (Unitless, (:*))
import Units qualified
import VectorCurve2d qualified

data Region2d (coordinateSystem :: CoordinateSystem)
  = Region2d (Loop coordinateSystem) (List (Loop coordinateSystem))

type Loop (coordinateSystem :: CoordinateSystem) =
  NonEmpty (Curve2d coordinateSystem)

data BuildError
  = EmptyRegion
  | RegionBoundaryHasGaps
  | RegionBoundaryIntersectsItself
  | MultipleDisjointRegions
  | TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, ErrorMessage)

boundedBy ::
  Tolerance units =>
  List (Curve2d (space @ units)) ->
  Result BuildError (Region2d (space @ units))
boundedBy curves = do
  () <- checkForInnerIntersection curves
  loops <- connect curves
  classifyLoops loops

checkForInnerIntersection ::
  Tolerance units =>
  List (Curve2d (space @ units)) ->
  Result BuildError ()
checkForInnerIntersection [] = Ok ()
checkForInnerIntersection (first : rest) = do
  () <- checkCurveForInnerIntersection first rest
  checkForInnerIntersection rest

checkCurveForInnerIntersection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BuildError ()
checkCurveForInnerIntersection _ [] = Ok ()
checkCurveForInnerIntersection curve (first : rest) = do
  () <- checkCurvesForInnerIntersection curve first
  checkCurveForInnerIntersection curve rest

checkCurvesForInnerIntersection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result BuildError ()
checkCurvesForInnerIntersection curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    Error (Curve2d.CurvesOverlap _) ->
      Error RegionBoundaryIntersectsItself
    Error Curve2d.TangentIntersectionAtDegeneratePoint ->
      Error TangentIntersectionAtDegeneratePoint
    Ok intersections
      | List.all isEndpointIntersection intersections -> Ok ()
      | otherwise -> Error RegionBoundaryIntersectsItself

isEndpointIntersection :: Intersection -> Bool
isEndpointIntersection (Intersection u1 u2 _ _) = isEndpoint u1 && isEndpoint u2

isEndpoint :: Float -> Bool
isEndpoint u = u == 0.0 || u == 1.0

connect ::
  Tolerance units =>
  List (Curve2d (space @ units)) ->
  Result BuildError (List (Loop (space @ units)))
connect [] = Ok []
connect (first : rest) = do
  (loop, remainingCurves) <- buildLoop (startLoop first) rest
  remainingLoops <- connect remainingCurves
  Ok (loop : remainingLoops)

data PartialLoop coordinateSystem
  = PartialLoop
      (Point2d coordinateSystem)
      (NonEmpty (Curve2d coordinateSystem))
      (Point2d coordinateSystem)

buildLoop ::
  Tolerance units =>
  PartialLoop (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BuildError (Loop (space @ units), List (Curve2d (space @ units)))
buildLoop partialLoop@(PartialLoop currentStart currentCurves loopEnd) remainingCurves
  | currentStart ~= loopEnd = Ok (currentCurves, remainingCurves)
  | otherwise = do
      (updatedPartialLoop, updatedRemainingCurves) <- extendPartialLoop partialLoop remainingCurves
      buildLoop updatedPartialLoop updatedRemainingCurves

extendPartialLoop ::
  Tolerance units =>
  PartialLoop (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BuildError (PartialLoop (space @ units), List (Curve2d (space @ units)))
extendPartialLoop (PartialLoop currentStart currentCurves loopEnd) curves =
  case List.partition (hasEndpoint currentStart) curves of
    ([], _) -> Error RegionBoundaryHasGaps
    ([curve], remaining) ->
      let newCurve =
            if Curve2d.endPoint curve ~= currentStart
              then curve
              else Curve2d.reverse curve
          newStart = Curve2d.startPoint newCurve
          updatedCurves = NonEmpty.prepend newCurve currentCurves
       in Ok (PartialLoop newStart updatedCurves loopEnd, remaining)
    (_ : _ : _, _) -> Error RegionBoundaryIntersectsItself

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
classify point curves
  | NonEmpty.any (Curve2d.passesThrough point) curves = Nothing
  | otherwise = Just (classifyNonBoundary point curves)

fluxIntegral :: Point2d (space @ units) -> Curve2d (space @ units) -> Estimate Unitless
fluxIntegral point curve =
  let displacement = Units.generalize (point - curve)
      firstDerivative = Units.generalize (Curve2d.derivative curve)
      integrand = (firstDerivative >< displacement) / VectorCurve2d.squaredMagnitude displacement
   in Curve1d.integral integrand

totalFlux :: Point2d (space @ units) -> Loop (space @ units) -> Estimate Unitless
totalFlux point loop =
  Estimate.sum (List.map (fluxIntegral point) (NonEmpty.toList loop))

classifyNonBoundary ::
  Tolerance units =>
  Point2d (space @ units) ->
  Loop (space @ units) ->
  Sign
classifyNonBoundary point loop =
  let flux = Estimate.satisfy containmentIsDeterminate (totalFlux point loop)
   in if Range.includes Qty.zero flux then Negative else Positive

bothPossibleFluxValues :: Range Unitless
bothPossibleFluxValues = Range.from 0.0 Float.tau

containmentIsDeterminate :: Range Unitless -> Bool
containmentIsDeterminate flux = not (Range.contains bothPossibleFluxValues flux)

classifyLoops ::
  Tolerance units =>
  List (Loop (space @ units)) ->
  Result BuildError (Region2d (space @ units))
classifyLoops [] = Error EmptyRegion
classifyLoops (NonEmpty loops) = do
  let (largestLoop, smallerLoops) = pickLargestLoop loops
  let outerLoop' = fixSign Positive largestLoop
  let innerLoops' = List.map (fixSign Negative) smallerLoops
  if List.all (loopIsInside outerLoop') innerLoops'
    then Ok (Region2d outerLoop' innerLoops')
    else Error MultipleDisjointRegions

fixSign :: Tolerance units => Sign -> Loop (space @ units) -> Loop (space @ units)
fixSign desiredSign loop =
  let ?tolerance = Qty.squared (Units.generalize ?tolerance)
   in if Estimate.sign (loopSignedArea loop) == desiredSign then loop else reverseLoop loop

reverseLoop :: Loop (space @ units) -> Loop (space @ units)
reverseLoop loop = NonEmpty.reverseMap Curve2d.reverse loop

pickLargestLoop ::
  Tolerance units =>
  NonEmpty (Loop (space @ units)) ->
  (Loop (space @ units), List (Loop (space @ units)))
pickLargestLoop loops =
  let ?tolerance = Qty.squared (Units.generalize ?tolerance)
   in Estimate.pickLargestBy loopSignedArea loops

loopSignedArea :: Loop (space @ units) -> Estimate (units :* units)
loopSignedArea loop =
  let referencePoint = Units.generalize (Curve2d.startPoint (NonEmpty.first loop))
   in NonEmpty.toList loop
        |> List.map (Units.generalize >> areaIntegral referencePoint)
        |> Estimate.sum

areaIntegral ::
  Units.Squared units1 units2 =>
  Point2d (space @ units1) ->
  Curve2d (space @ units1) ->
  Estimate units2
areaIntegral referencePoint curve =
  let displacement = curve - referencePoint
      y = VectorCurve2d.yComponent displacement
      dx = Curve1d.derivative (VectorCurve2d.xComponent displacement)
   in -(Curve1d.integral (y * dx))

loopIsInside :: Tolerance units => Loop (space @ units) -> Loop (space @ units) -> Bool
loopIsInside outer inner =
  let testPoint = Curve2d.startPoint (NonEmpty.first inner)
   in case classify testPoint outer of
        Nothing -> True -- Shouldn't happen, loops should be guaranteed not to be touching by this point
        Just Positive -> True
        Just Negative -> False

boundingBox :: Region2d (space @ units) -> BoundingBox2d (space @ units)
boundingBox region =
  NonEmpty.reduceLeft BoundingBox2d.aggregate2 $
    NonEmpty.map Curve2d.boundingBox (outerLoop region)

area :: Units.Squared units1 units2 => Region2d (space @ units1) -> Estimate units2
area region =
  let referencePoint = Curve2d.startPoint (NonEmpty.first (outerLoop region))
   in Estimate.sum (List.map (areaIntegral referencePoint) (NonEmpty.toList (boundaryCurves region)))
