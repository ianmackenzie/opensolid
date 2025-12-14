module OpenSolid.Region2d
  ( Region2d
  , EmptyRegion (EmptyRegion)
  , boundedBy
  , unitSquare
  , rectangle
  , circle
  , polygon
  , outerLoop
  , innerLoops
  , boundaryLoops
  , boundaryCurves
  , placeIn
  , relativeTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  , convert
  , unconvert
  , classify
  , contains
  , bounds
  , area
  , toMesh
  , fillet
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CDT qualified as CDT
import OpenSolid.Circle2d (Circle2d)
import OpenSolid.Circle2d qualified as Circle2d
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint (IntersectionPoint))
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
import OpenSolid.Curve2d.Intersections qualified as Curve2d.Intersections
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Line2d (Line2d (Line2d))
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Mesh (Mesh)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Polygon2d (Polygon2d)
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Polyline2d qualified as Polyline2d
import OpenSolid.Polymorphic.Point2d (Point2d (Point2d))
import OpenSolid.Polymorphic.Vector2d (Vector2d)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2d.BoundedBy qualified as BoundedBy
import OpenSolid.Resolution (Resolution)
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

type role Region2d nominal nominal

-- | A closed 2D region (possibly with holes), defined by a set of boundary curves.
data Region2d units space
  = Region2d (Loop units space) (List (Loop units space))

type Loop units space =
  NonEmpty (Curve2d units space)

instance FFI (Region2d Meters FFI.Space) where
  representation = FFI.classRepresentation "Region2d"

instance FFI (Region2d Unitless UvSpace) where
  representation = FFI.classRepresentation "UvRegion"

instance HasUnits (Region2d units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (Region2d unitsA space1) (Region2d unitsB space2)
  where
  coerce (Region2d outer inners) =
    Region2d
      (NonEmpty.map Units.coerce outer)
      (List.map (NonEmpty.map Units.coerce) inners)

{-| Create a region bounded by the given curves.

The curves may be given in any order,
do not need to have consistent directions
and can form multiple separate loops if the region has holes.
However, the curves must not overlap or intersect (other than at endpoints)
and there must not be any gaps between them.
-}
boundedBy ::
  Tolerance units =>
  List (Curve2d units space) ->
  Result BoundedBy.Error (Region2d units space)
boundedBy curves = do
  checkForInnerIntersection curves
  loops <- connect curves
  classifyLoops loops

-- | The unit square in UV space.
unitSquare :: Region2d Unitless UvSpace
unitSquare = Tolerance.using Quantity.zero do
  case rectangle (Bounds2d Bounds.unitInterval Bounds.unitInterval) of
    Ok region -> region
    Error EmptyRegion -> throw (InternalError "Constructing unit square region should not fail")

data EmptyRegion = EmptyRegion deriving (Eq, Show)

{-| Create a rectangular region.

Fails if the given bounds are empty
(zero area, i.e. zero width in either direction).
-}
rectangle ::
  Tolerance units =>
  Bounds2d units space ->
  Result EmptyRegion (Region2d units space)
rectangle (Bounds2d xBounds yBounds) =
  if Bounds.width xBounds ~= Quantity.zero || Bounds.width yBounds ~= Quantity.zero
    then Error EmptyRegion
    else Ok do
      let Bounds x1 x2 = xBounds
      let Bounds y1 y2 = yBounds
      let p11 = Point2d x1 y1
      let p12 = Point2d x1 y2
      let p21 = Point2d x2 y1
      let p22 = Point2d x2 y2
      let edges =
            NonEmpty.four
              (Curve2d.lineFrom p11 p21)
              (Curve2d.lineFrom p21 p22)
              (Curve2d.lineFrom p22 p12)
              (Curve2d.lineFrom p12 p11)
      Region2d edges []

-- | Create a region from the given circle.
circle :: Tolerance units => Circle2d units space -> Result EmptyRegion (Region2d units space)
circle givenCircle =
  if Circle2d.diameter givenCircle ~= Quantity.zero
    then Error EmptyRegion
    else Ok (Region2d (NonEmpty.one (Curve2d.circle givenCircle)) [])

-- | Create a region from the given polygon.
polygon :: Tolerance units => Polygon2d units space -> Result BoundedBy.Error (Region2d units space)
polygon givenPolygon = do
  let toCurve (Line2d p1 p2) = if p1 ~= p2 then Nothing else Just (Curve2d.lineFrom p1 p2)
  boundedBy (NonEmpty.filterMap toCurve (Polygon2d.edges givenPolygon))

{-| Fillet a region at the given corner points, with the given radius.

Fails if any of the given points are not actually corner points of the region
(within the given tolerance),
or if it is not possible to solve for a given fillet
(e.g. if either of the adjacent edges is not long enough).
-}
fillet ::
  Tolerance units =>
  List (Point2d units space) ->
  "radius" ::: Quantity units ->
  Region2d units space ->
  Result Text (Region2d units space)
fillet points (Named radius) region = do
  let initialCurves = NonEmpty.toList (boundaryCurves region)
  filletedCurves <- Result.orFail (Result.foldl (addFillet radius) initialCurves points)
  Result.orFail (boundedBy filletedCurves)

addFillet ::
  Tolerance units =>
  Quantity units ->
  List (Curve2d units space) ->
  Point2d units space ->
  Result Text (List (Curve2d units space))
addFillet radius curves point = do
  let couldNotFindPointToFillet = Error "Could not find point to fillet"
  let couldNotSolveForFilletLocation = Error "Could not solve for fillet location"
  let curveIncidences = List.map (curveIncidence point) curves
  let incidentCurves =
        curveIncidences
          & List.filterMap incidentCurve
          -- By this point we should have exactly two curves
          -- (one 'incoming' and one 'outgoing');
          -- sort them by the negation of the incident parameter value
          -- so that the curve with parameter value 1 (the incoming curve) is first
          -- is and the curve with parameter value 0 (the outgoing curve) is second
          & List.sortBy (negative . Pair.second)
          & List.map Pair.first
  let otherCurves = List.filterMap nonIncidentCurve curveIncidences
  case incidentCurves of
    [] -> couldNotFindPointToFillet
    List.One{} -> couldNotFindPointToFillet
    List.ThreeOrMore{} -> couldNotFindPointToFillet
    List.Two firstCurve secondCurve -> do
      firstTangent <- Result.orFail (Curve2d.tangentDirection firstCurve)
      secondTangent <- Result.orFail (Curve2d.tangentDirection secondCurve)
      let firstEndDirection = DirectionCurve2d.endValue firstTangent
      let secondStartDirection = DirectionCurve2d.startValue secondTangent
      let cornerAngle = Direction2d.angleFrom firstEndDirection secondStartDirection
      let offset = Quantity.sign cornerAngle .*. Quantity.abs radius
      let firstOffsetDisplacement =
            VectorCurve2d.rotateBy Angle.quarterTurn (offset .*. firstTangent)
      let secondOffsetDisplacement =
            VectorCurve2d.rotateBy Angle.quarterTurn (offset .*. secondTangent)
      let firstOffsetCurve = firstCurve .+. firstOffsetDisplacement
      let secondOffsetCurve = secondCurve .+. secondOffsetDisplacement
      maybeIntersections <- Result.orFail (Curve2d.intersections firstOffsetCurve secondOffsetCurve)
      case maybeIntersections of
        Nothing -> couldNotSolveForFilletLocation
        Just Curve2d.OverlappingSegments{} -> couldNotSolveForFilletLocation
        Just (Curve2d.IntersectionPoints intersectionPoints) -> do
          let intersection1 = NonEmpty.maximumBy (.t1) intersectionPoints
          let intersection2 = NonEmpty.minimumBy (.t2) intersectionPoints
          if intersection1 /= intersection2
            then couldNotSolveForFilletLocation
            else do
              let IntersectionPoint{t1, t2} = intersection1
              let centerPoint = Curve2d.evaluate firstOffsetCurve t1
              let startPoint = Curve2d.evaluate firstCurve t1
              let sweptAngle =
                    Direction2d.angleFrom
                      (DirectionCurve2d.evaluate firstTangent t1)
                      (DirectionCurve2d.evaluate secondTangent t2)
              let filletArc = Curve2d.sweptArc centerPoint startPoint sweptAngle
              let trimmedFirstCurve = firstCurve `compose` Curve.line 0 t1
              let trimmedSecondCurve = secondCurve `compose` Curve.line t2 1
              Ok (filletArc : trimmedFirstCurve : trimmedSecondCurve : otherCurves)

curveIncidence ::
  Tolerance units =>
  Point2d units space ->
  Curve2d units space ->
  (Curve2d units space, Maybe Number)
curveIncidence point curve
  | point ~= curve.startPoint = (curve, Just 0)
  | point ~= curve.endPoint = (curve, Just 1)
  | otherwise = (curve, Nothing)

incidentCurve :: (Curve2d units space, Maybe Number) -> Maybe (Curve2d units space, Number)
incidentCurve (curve, maybeIncidence) = Maybe.map (curve,) maybeIncidence

nonIncidentCurve :: (Curve2d units space, Maybe Number) -> Maybe (Curve2d units space)
nonIncidentCurve (curve, Nothing) = Just curve
nonIncidentCurve (_, Just _) = Nothing

checkForInnerIntersection ::
  Tolerance units =>
  List (Curve2d units space) ->
  Result BoundedBy.Error ()
checkForInnerIntersection [] = Ok ()
checkForInnerIntersection (first : rest) = do
  checkCurveForInnerIntersection first rest
  checkForInnerIntersection rest

checkCurveForInnerIntersection ::
  Tolerance units =>
  Curve2d units space ->
  List (Curve2d units space) ->
  Result BoundedBy.Error ()
checkCurveForInnerIntersection _ [] = Ok ()
checkCurveForInnerIntersection curve (first : rest) = do
  checkCurvesForInnerIntersection curve first
  checkCurveForInnerIntersection curve rest

checkCurvesForInnerIntersection ::
  Tolerance units =>
  Curve2d units space ->
  Curve2d units space ->
  Result BoundedBy.Error ()
checkCurvesForInnerIntersection curve1 curve2 =
  case Curve2d.intersections curve1 curve2 of
    -- We can ignore cases where either curve is actually a point,
    -- since we'll still find any inner intersections
    -- when we check with the *neighbours* of those degenerate curves
    Error Curve2d.Intersections.FirstCurveIsPoint -> Ok ()
    Error Curve2d.Intersections.SecondCurveIsPoint -> Ok ()
    Error Curve2d.Intersections.BothCurvesArePoints -> Ok ()
    -- Any overlap between boundary curves is bad
    Ok (Just (Curve2d.OverlappingSegments _)) ->
      Error BoundedBy.BoundaryIntersectsItself
    -- If there are no intersections at all then we're good!
    Ok Nothing -> Ok ()
    -- Otherwise, make sure curves only intersect (meet) at endpoints
    Ok (Just (Curve2d.IntersectionPoints intersectionPoints)) ->
      if NonEmpty.allSatisfy isEndpointIntersection intersectionPoints
        then Ok ()
        else Error BoundedBy.BoundaryIntersectsItself

isEndpointIntersection :: IntersectionPoint -> Bool
isEndpointIntersection intersectionPoint = do
  Curve.isEndpoint intersectionPoint.t1 && Curve.isEndpoint intersectionPoint.t2

connect ::
  Tolerance units =>
  List (Curve2d units space) ->
  Result BoundedBy.Error (List (Loop units space))
connect [] = Ok []
connect (first : rest) = do
  (loop, remainingCurves) <- buildLoop (startLoop first) rest
  remainingLoops <- connect remainingCurves
  Ok (loop : remainingLoops)

data PartialLoop units space
  = PartialLoop
      (Point2d units space)
      (NonEmpty (Curve2d units space))
      (Point2d units space)

buildLoop ::
  Tolerance units =>
  PartialLoop units space ->
  List (Curve2d units space) ->
  Result BoundedBy.Error (Loop units space, List (Curve2d units space))
buildLoop partialLoop@(PartialLoop currentStart currentCurves loopEnd) remainingCurves
  | currentStart ~= loopEnd = Ok (currentCurves, remainingCurves)
  | otherwise = do
      (updatedPartialLoop, updatedRemainingCurves) <- extendPartialLoop partialLoop remainingCurves
      buildLoop updatedPartialLoop updatedRemainingCurves

extendPartialLoop ::
  Tolerance units =>
  PartialLoop units space ->
  List (Curve2d units space) ->
  Result BoundedBy.Error (PartialLoop units space, List (Curve2d units space))
extendPartialLoop (PartialLoop currentStart currentCurves loopEnd) curves =
  case List.partition (hasEndpoint currentStart) curves of
    ([], _) -> Error BoundedBy.BoundaryHasGaps
    (List.One curve, remaining) -> do
      let newCurve = if curve.endPoint ~= currentStart then curve else Curve2d.reverse curve
      let updatedCurves = NonEmpty.push newCurve currentCurves
      Ok (PartialLoop newCurve.startPoint updatedCurves loopEnd, remaining)
    (List.TwoOrMore, _) -> Error BoundedBy.BoundaryIntersectsItself

hasEndpoint :: Tolerance units => Point2d units space -> Curve2d units space -> Bool
hasEndpoint point curve = point ~= curve.startPoint || point ~= curve.endPoint

startLoop :: Curve2d units space -> PartialLoop units space
startLoop curve = PartialLoop curve.startPoint (NonEmpty.one curve) curve.endPoint

outerLoop :: Region2d units space -> NonEmpty (Curve2d units space)
outerLoop (Region2d loop _) = loop

innerLoops :: Region2d units space -> List (NonEmpty (Curve2d units space))
innerLoops (Region2d _ loops) = loops

boundaryLoops :: Region2d units space -> NonEmpty (NonEmpty (Curve2d units space))
boundaryLoops region = outerLoop region :| innerLoops region

boundaryCurves :: Region2d units space -> NonEmpty (Curve2d units space)
boundaryCurves region = NonEmpty.concat (boundaryLoops region)

placeIn :: Frame2d units global local -> Region2d units local -> Region2d units global
placeIn frame (Region2d outer inners) = do
  let transformLoop = NonEmpty.map (Curve2d.placeIn frame)
  Region2d (transformLoop outer) (List.map transformLoop inners)

relativeTo :: Frame2d units global local -> Region2d units global -> Region2d units local
relativeTo frame region = placeIn (Frame2d.inverse frame) region

transformBy :: Transform2d tag units space -> Region2d units space -> Region2d units space
transformBy transform (Region2d outer inners) = do
  let transformLoop =
        case Transform2d.handedness transform of
          Positive -> NonEmpty.map (Curve2d.transformBy transform)
          Negative -> NonEmpty.reverseMap (Curve2d.reverse . Curve2d.transformBy transform)
  Region2d (transformLoop outer) (List.map transformLoop inners)

translateBy ::
  Vector2d units space ->
  Region2d units space ->
  Region2d units space
translateBy = Transform2d.translateByImpl transformBy

translateIn ::
  Direction2d space ->
  Quantity units ->
  Region2d units space ->
  Region2d units space
translateIn = Transform2d.translateInImpl transformBy

translateAlong ::
  Axis2d units space ->
  Quantity units ->
  Region2d units space ->
  Region2d units space
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround ::
  Point2d units space ->
  Angle ->
  Region2d units space ->
  Region2d units space
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2d units space ->
  Region2d units space ->
  Region2d units space
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

scaleAbout ::
  Point2d units space ->
  Number ->
  Region2d units space ->
  Region2d units space
scaleAbout = Transform2d.scaleAboutImpl transformBy

scaleAlong ::
  Axis2d units space ->
  Number ->
  Region2d units space ->
  Region2d units space
scaleAlong = Transform2d.scaleAlongImpl transformBy

convert ::
  Quantity (units2 ?/? units1) ->
  Region2d units1 space ->
  Region2d units2 space
convert factor (Region2d outer inners) = do
  let transform =
        case Quantity.sign factor of
          Positive -> NonEmpty.map (Curve2d.convert factor)
          Negative -> NonEmpty.reverseMap (Curve2d.reverse . Curve2d.convert factor)
  Region2d (transform outer) (List.map transform inners)

unconvert ::
  Quantity (units2 ?/? units1) ->
  Region2d units2 space ->
  Region2d units1 space
unconvert factor region = convert (Units.simplify (1 /? factor)) region

contains :: Tolerance units => Point2d units space -> Region2d units space -> Bool
contains point region =
  case classify point (boundaryCurves region) of
    Nothing -> True -- Point on boundary is considered contained
    Just Positive -> True
    Just Negative -> False

classify ::
  Tolerance units =>
  Point2d units space ->
  NonEmpty (Curve2d units space) ->
  Maybe Sign
classify point curves =
  if NonEmpty.anySatisfy (intersects point) curves
    then Nothing
    else Just (classifyNonBoundary point curves)

fluxIntegral ::
  Tolerance units =>
  Point2d units space ->
  Curve2d units space ->
  Estimate Unitless
fluxIntegral point curve = do
  let displacement = point .-. curve
  -- By this point we've already checked whether the poing is *on* the curve,
  -- so the Curve.unsafeQuotient call should be OK
  -- (if the point is not on the curve, then the displacement will always be non-zero)
  let integrand =
        Tolerance.using (Quantity.squared_ ?tolerance) $
          Curve.unsafeQuotient
            (curve.derivative `cross_` displacement)
            (VectorCurve2d.squaredMagnitude_ displacement)
  Curve.integrate integrand

totalFlux :: Tolerance units => Point2d units space -> Loop units space -> Estimate Unitless
totalFlux point loop = Estimate.sum (NonEmpty.map (fluxIntegral point) loop)

classifyNonBoundary :: Tolerance units => Point2d units space -> Loop units space -> Sign
classifyNonBoundary point loop = do
  let flux = Estimate.satisfy containmentIsDeterminate (totalFlux point loop)
  if Bounds.includes Quantity.zero flux then Negative else Positive

bothPossibleFluxValues :: Bounds Unitless
bothPossibleFluxValues = Bounds 0 Number.twoPi

containmentIsDeterminate :: Bounds Unitless -> Bool
containmentIsDeterminate flux = not (Bounds.contains bothPossibleFluxValues flux)

classifyLoops ::
  Tolerance units =>
  List (Loop units space) ->
  Result BoundedBy.Error (Region2d units space)
classifyLoops [] = Error BoundedBy.EmptyRegion
classifyLoops (NonEmpty loops) = do
  let (largestLoop, smallerLoops) = pickLargestLoop loops
  let outerLoopCandidate = fixSign Positive largestLoop
  let innerLoopCandidates = List.map (fixSign Negative) smallerLoops
  if List.allSatisfy (loopIsInside outerLoopCandidate) innerLoopCandidates
    then Ok (Region2d outerLoopCandidate innerLoopCandidates)
    else Error BoundedBy.MultipleDisjointRegions

fixSign :: Tolerance units => Sign -> Loop units space -> Loop units space
fixSign desiredSign loop =
  Tolerance.using (Quantity.squared_ ?tolerance) do
    if Estimate.sign (loopSignedArea_ loop) == desiredSign then loop else reverseLoop loop

reverseLoop :: Loop units space -> Loop units space
reverseLoop loop = NonEmpty.reverseMap Curve2d.reverse loop

pickLargestLoop ::
  Tolerance units =>
  NonEmpty (Loop units space) ->
  (Loop units space, List (Loop units space))
pickLargestLoop loops =
  Tolerance.using (Quantity.squared_ ?tolerance) do
    Estimate.pickLargestBy loopSignedArea_ loops

loopSignedArea_ :: Loop units space -> Estimate (units ?*? units)
loopSignedArea_ loop = do
  let referencePoint = Curve2d.startPoint (NonEmpty.first loop)
  let edgeIntegrals = NonEmpty.map (areaIntegral_ referencePoint) loop
  Estimate.sum edgeIntegrals

areaIntegral ::
  Units.Squared units1 units2 =>
  Point2d units1 space ->
  Curve2d units1 space ->
  Estimate units2
areaIntegral referencePoint curve =
  Units.specialize (areaIntegral_ referencePoint curve)

areaIntegral_ :: Point2d units space -> Curve2d units space -> Estimate (units ?*? units)
areaIntegral_ referencePoint curve = do
  let displacement = curve .-. referencePoint
  let y = displacement.yComponent
  let dx = displacement.xComponent.derivative
  negative (Curve.integrate (y ?*? dx))

loopIsInside :: Tolerance units => Loop units space -> Loop units space -> Bool
loopIsInside outer inner = do
  let testPoint = Curve2d.startPoint (NonEmpty.first inner)
  case classify testPoint outer of
    Nothing -> True -- Shouldn't happen, loops should be guaranteed not to be touching by this point
    Just Positive -> True
    Just Negative -> False

bounds :: Region2d units space -> Bounds2d units space
bounds region = do
  let outerEdgeBoundingBoxes = NonEmpty.map Curve2d.bounds (outerLoop region)
  NonEmpty.reduce Bounds2d.aggregate2 outerEdgeBoundingBoxes

area :: Units.Squared units1 units2 => Region2d units1 space -> Estimate units2
area region = do
  let referencePoint = Curve2d.startPoint (NonEmpty.first (outerLoop region))
  let edgeIntegrals = NonEmpty.map (areaIntegral referencePoint) (boundaryCurves region)
  Estimate.sum edgeIntegrals

toMesh :: Resolution units -> Region2d units space -> Mesh (Point2d units space)
toMesh resolution region = do
  let vertexLoops = NonEmpty.map (toVertexLoop resolution) (boundaryLoops region)
  CDT.unsafe vertexLoops []

toVertexLoop ::
  Resolution units ->
  NonEmpty (Curve2d units space) ->
  NonEmpty (Point2d units space)
toVertexLoop resolution loop = do
  let trailingVertices curve = do
        let polyline = Curve2d.toPolyline resolution curve
        NonEmpty.rest (Polyline2d.vertices polyline)
  let allVertices = List.combine trailingVertices (NonEmpty.toList loop)
  case allVertices of
    NonEmpty vertices -> vertices
    [] -> throw (InternalError "Should always have at least one vertex")
