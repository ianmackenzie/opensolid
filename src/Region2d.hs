{-# OPTIONS_GHC -Wwarn #-}

module Region2d
  ( Region2d
  , BuildError (..)
  , boundedBy
  , outerLoop
  , innerLoops
  , boundaryCurves
  , classify
  , contains
  )
where

import CoordinateSystem (Units)
import Curve2d (Curve2d)
import Curve2d qualified
import Curve2d.Intersection (Intersection (Intersection))
import Domain (Domain)
import Domain qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Qty qualified
import Range (Range)
import Range qualified

data Region2d (coordinateSystem :: CoordinateSystem)
  = Region2d (Loop coordinateSystem) (List (Loop coordinateSystem))

type Loop (coordinateSystem :: CoordinateSystem) =
  List (Curve2d coordinateSystem)

data BuildError
  = RegionBoundaryHasGaps
  | RegionBoundaryIntersectsItself
  | TangentIntersectionAtDegeneratePoint
  deriving (Eq, Show, ErrorMessage)

boundedBy ::
  Tolerance units =>
  List (Curve2d (space @ units)) ->
  Result BuildError (Region2d (space @ units))
boundedBy curves = do
  () <- checkForInnerIntersection curves
  loops <- connect curves
  notImplemented

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
    Error (Curve2d.CurvesOverlap _) -> Error RegionBoundaryIntersectsItself
    Error Curve2d.TangentIntersectionAtDegeneratePoint -> Error TangentIntersectionAtDegeneratePoint
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
      (List (Curve2d coordinateSystem))
      (Point2d coordinateSystem)

buildLoop ::
  Tolerance units =>
  PartialLoop (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BuildError (Loop (space @ units), List (Curve2d (space @ units)))
buildLoop partialLoop remainingCurves = do
  extensionResult <- extendPartialLoop partialLoop remainingCurves
  case extensionResult of
    UpdatedPartialLoop updatedPartialLoop updatedRemainingCurves ->
      buildLoop updatedPartialLoop updatedRemainingCurves
    CompletedLoop loop updatedRemainingCurves ->
      Ok (loop, updatedRemainingCurves)

data LoopExtensionResult (coordinateSystem :: CoordinateSystem)
  = UpdatedPartialLoop (PartialLoop coordinateSystem) (List (Curve2d coordinateSystem))
  | CompletedLoop (Loop coordinateSystem) (List (Curve2d coordinateSystem))

extendPartialLoop ::
  Tolerance units =>
  PartialLoop (space @ units) ->
  List (Curve2d (space @ units)) ->
  Result BuildError (LoopExtensionResult (space @ units))
extendPartialLoop (PartialLoop currentStart currentCurves loopEnd) curves =
  case List.partition (hasEndpoint currentStart) curves of
    ([], _) -> Error RegionBoundaryHasGaps
    ([curve], remaining) ->
      let newCurve =
            if Curve2d.endPoint curve ~= currentStart
              then curve
              else Curve2d.reverse curve
          newStart = Curve2d.startPoint newCurve
          updatedCurves = newCurve : currentCurves
       in if newStart ~= loopEnd
            then Ok (CompletedLoop updatedCurves remaining)
            else Ok (UpdatedPartialLoop (PartialLoop newStart updatedCurves loopEnd) remaining)
    (_ : _ : _, _) -> Error RegionBoundaryIntersectsItself

hasEndpoint :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Bool
hasEndpoint point curve =
  Curve2d.startPoint curve ~= point || Curve2d.endPoint curve ~= point

startLoop :: Curve2d (space @ units) -> PartialLoop (space @ units)
startLoop curve = PartialLoop (Curve2d.startPoint curve) [curve] (Curve2d.endPoint curve)

outerLoop :: Region2d (space @ units) -> List (Curve2d (space @ units))
outerLoop (Region2d loop _) = loop

innerLoops :: Region2d (space @ units) -> List (List (Curve2d (space @ units)))
innerLoops (Region2d _ loops) = loops

boundaryCurves :: Region2d (space @ units) -> List (Curve2d (space @ units))
boundaryCurves region = List.concat (outerLoop region : innerLoops region)

contains :: Tolerance units => Point2d (space @ units) -> Region2d (space @ units) -> Bool
contains point region =
  case classify point (boundaryCurves region) of
    Nothing -> True -- Point on boundary is considered contained
    Just Positive -> True
    Just Negative -> False

classify ::
  Tolerance units =>
  Point2d (space @ units) ->
  List (Curve2d (space @ units)) ->
  Maybe Sign
classify point curves
  | List.any (Curve2d.passesThrough point) curves = Nothing
  | otherwise = Just (classifyNonBoundary point curves)

data ClassificationSegment (coordinateSystem :: CoordinateSystem)
  = CurveSegment (Curve2d coordinateSystem) Domain (Range (Units coordinateSystem))

classificationSegment ::
  Point2d (space @ units) ->
  Domain ->
  Curve2d (space @ units) ->
  ClassificationSegment (space @ units)
classificationSegment point domain curve =
  CurveSegment curve domain (computeSignedDistanceBounds point curve domain)

classifyNonBoundary ::
  Tolerance units =>
  Point2d (space @ units) ->
  List (Curve2d (space @ units)) ->
  Sign
classifyNonBoundary point curves = go (List.map (classificationSegment point Domain.unit) curves)
 where
  segmentDistanceUpperBound (CurveSegment _ _ signedDistanceRange) =
    Range.maxValue (Range.abs signedDistanceRange)
  isCandidate distanceUpperBound (CurveSegment _ _ signedDistanceRange) =
    Range.minValue (Range.abs signedDistanceRange) <= distanceUpperBound
  go [] = Negative
  go (NonEmpty segments)
    | List.all (== Resolved Positive) filteredSigns = Positive
    | List.all (== Resolved Negative) filteredSigns = Negative
    | otherwise = go (bisectSegments point filteredSegments)
   where
    distanceUpperBound = NonEmpty.minimum (NonEmpty.map segmentDistanceUpperBound segments)
    filteredSegments = NonEmpty.filter (isCandidate distanceUpperBound) segments
    filteredSigns = List.map resolvedSign filteredSegments

computeSignedDistanceBounds ::
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Domain ->
  Range units
computeSignedDistanceBounds point curve domain =
  let tangentBounds = Curve2d.tangentBounds domain curve
      displacementBounds = point - Curve2d.segmentBounds domain curve
   in tangentBounds >< displacementBounds

resolvedSign :: ClassificationSegment (space @ units) -> Fuzzy Sign
resolvedSign (CurveSegment _ _ signedDistanceBounds) =
  let resolution = Range.resolution signedDistanceBounds
   in if Qty.abs resolution >= 0.5 then Resolved (Qty.sign resolution) else Unresolved

bisectSegments ::
  Point2d (space @ units) ->
  List (ClassificationSegment (space @ units)) ->
  List (ClassificationSegment (space @ units))
bisectSegments _ [] = []
bisectSegments point (CurveSegment curve domain _ : rest) =
  let (left, right) = Range.bisect domain
   in classificationSegment point left curve : classificationSegment point right curve : bisectSegments point rest
