module OpenSolid.Region2d
  ( Region2d
  , EmptyRegion (EmptyRegion)
  , boundedBy
  , unitSquare
  , rectangle
  , circle
  , hexagon
  , inscribedPolygon
  , circumscribedPolygon
  , polygon
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
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve2d.IntersectionPoint qualified as IntersectionPoint
-- import OpenSolid.Curve2d.IntersectionPoint (IntersectionPoint)
-- import OpenSolid.Curve2d.IntersectionPoint qualified as Curve2d.IntersectionPoint
-- import OpenSolid.Curve2d.Intersections qualified as Curve2d.Intersections
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Error qualified as Error
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.LineSegment2d (LineSegment2d (LineSegment2d))
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Mesh (Mesh)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Polyline2d qualified as Polyline2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Region2d.BoundedBy qualified as BoundedBy
import OpenSolid.Resolution (Resolution)
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Try qualified as Try
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.Vertex2d (Vertex2d)

type role Region2d nominal

-- | A closed 2D region (possibly with holes), defined by a set of boundary curves.
data Region2d (coordinateSystem :: CoordinateSystem)
  = Region2d (Loop coordinateSystem) (List (Loop coordinateSystem))

type Loop (coordinateSystem :: CoordinateSystem) =
  NonEmpty (Curve2d coordinateSystem)

instance FFI (Region2d FFI.Coordinates) where
  representation = FFI.classRepresentation "Region2d"

instance FFI (Region2d UvCoordinates) where
  representation = FFI.classRepresentation "UvRegion"

instance HasUnits (Region2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Region2d (space1 @ unitsA)) (Region2d (space2 @ unitsB))
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
  List (Curve2d (space @ units)) ->
  Result BoundedBy.Error (Region2d (space @ units))
boundedBy curves = Result.do
  -- checkForInnerIntersection curves
  loops <- connect curves
  classifyLoops loops

-- | The unit square in UV space.
unitSquare :: Region2d UvCoordinates
unitSquare =
  case Tolerance.using Qty.zero (rectangle (Bounds2d Bounds.unitInterval Bounds.unitInterval)) of
    Success region -> region
    Failure EmptyRegion -> internalError "Constructing unit square region should not fail"

data EmptyRegion = EmptyRegion deriving (Eq, Show, Error.Message)

{-| Create a rectangular region.

Fails if the given bounds are empty
(zero area, i.e. zero width in either direction).
-}
rectangle ::
  Tolerance units =>
  Bounds2d (space @ units) ->
  Result EmptyRegion (Region2d (space @ units))
rectangle (Bounds2d xBounds yBounds) =
  if Bounds.width xBounds ~= Qty.zero || Bounds.width yBounds ~= Qty.zero
    then Failure EmptyRegion
    else Success do
      let Bounds x1 x2 = xBounds
      let Bounds y1 y2 = yBounds
      let p11 = Point2d x1 y1
      let p12 = Point2d x1 y2
      let p21 = Point2d x2 y1
      let p22 = Point2d x2 y2
      let edges =
            NonEmpty.four
              (Curve2d.line p11 p21)
              (Curve2d.line p21 p22)
              (Curve2d.line p22 p12)
              (Curve2d.line p12 p11)
      Region2d edges []

{-| Create a circular region.

Fails if the given dimeter is zero.
-}
circle ::
  Tolerance units =>
  ("centerPoint" ::: Point2d (space @ units), "diameter" ::: Qty units) ->
  Result EmptyRegion (Region2d (space @ units))
circle args =
  if args.diameter ~= Qty.zero
    then Failure EmptyRegion
    else Success (Region2d (NonEmpty.one (Curve2d.circle args)) [])

{-| Create a hexagon with the given center point and height.

The hexagon will be oriented such that its top and bottom edges are horizontal.
-}
hexagon ::
  Tolerance units =>
  ("centerPoint" ::: Point2d (space @ units), "height" ::: Qty units) ->
  Result EmptyRegion (Region2d (space @ units))
hexagon args =
  circumscribedPolygon 6 (#centerPoint args.centerPoint, #diameter args.height)

{-| Create a regular polygon with the given number of sides.

The polygon will be sized to fit within a circle with the given center point and diameter
(each polygon vertex will lie on the circle).
The polygon will be oriented such that its bottom-most edge is horizontal.
-}
inscribedPolygon ::
  Tolerance units =>
  Int ->
  ("centerPoint" ::: Point2d (space @ units), "diameter" ::: Qty units) ->
  Result EmptyRegion (Region2d (space @ units))
inscribedPolygon n args = do
  if args.diameter < Qty.zero || args.diameter ~= Qty.zero || n < 3
    then Failure EmptyRegion
    else Success do
      let radius = 0.5 * args.diameter
      let vertexAngles = Qty.midpoints (Angle.degrees -90.0) (Angle.degrees 270.0) n
      let vertex angle = args.centerPoint + Vector2d.polar radius angle
      let vertices = List.map vertex vertexAngles
      case polygon vertices of
        Success region -> region
        Failure _ ->
          internalError "Regular polygon construction with non-zero diameter should not fail"

{-| Create a regular polygon with the given number of sides.

The polygon will be sized so that
a circle with the given center point and diameter will just fit within the polygon
(each polygon edge will touch the circle at the edge's midpoint).
For a polygon with an even number of sides (square, hexagon, octagon etc.),
this means that the "width across flats" will be equal to the given circle diameter.
The polygon will be oriented such that its bottom-most edge is horizontal.
-}
circumscribedPolygon ::
  Tolerance units =>
  Int ->
  ("centerPoint" ::: Point2d (space @ units), "diameter" ::: Qty units) ->
  Result EmptyRegion (Region2d (space @ units))
circumscribedPolygon n args =
  inscribedPolygon n do
    #centerPoint args.centerPoint
    #diameter (args.diameter / Angle.cos (Angle.pi / Float.int n))

{-| Create a polygonal region from the given vertices.

The last vertex will be connected back to the first vertex automatically if needed
(you do not have to close the polygon manually, although it will still work if you do).
-}
polygon ::
  (Vertex2d vertex (space @ units), Tolerance units) =>
  List vertex ->
  Result BoundedBy.Error (Region2d (space @ units))
polygon vertexList = case vertexList of
  [] -> Failure BoundedBy.EmptyRegion
  NonEmpty vertices -> do
    let closedLoop = NonEmpty.push vertices.last vertices
    let segments = NonEmpty.successive LineSegment2d closedLoop
    let nonZeroLengthSegments = List.filter ((.length) >> (!= Qty.zero)) segments
    let toCurve segment = Curve2d.line segment.startPoint segment.endPoint
    boundedBy (List.map toCurve nonZeroLengthSegments)

{-| Fillet a region at the given corner points, with the given radius.

Fails if any of the given points are not actually corner points of the region
(within the given tolerance),
or if it is not possible to solve for a given fillet
(e.g. if either of the adjacent edges is not long enough).
-}
fillet ::
  Tolerance units =>
  List (Point2d (space @ units)) ->
  "radius" ::: Qty units ->
  Region2d (space @ units) ->
  Result Text (Region2d (space @ units))
fillet points (Named radius) region = Try.do
  let initialCurves = NonEmpty.toList region.boundaryCurves
  filletedCurves <- Result.foldl (addFillet radius) initialCurves points
  boundedBy filletedCurves

addFillet ::
  Tolerance units =>
  Qty units ->
  List (Curve2d (space @ units)) ->
  Point2d (space @ units) ->
  Result Text (List (Curve2d (space @ units)))
addFillet radius curves point = Try.do
  let couldNotFindPointToFillet = Failure "Could not find point to fillet"
  let couldNotSolveForFilletLocation = Failure "Could not solve for fillet location"
  let curveIncidences = List.map (curveIncidence point) curves
  let incidentCurves =
        curveIncidences
          |> Maybe.collect incidentCurve
          -- By this point we should have exactly two curves
          -- (one 'incoming' and one 'outgoing');
          -- sort them by the negation of the incident parameter value
          -- so that the curve with parameter value 1 (the incoming curve) is first
          -- is and the curve with parameter value 0 (the outgoing curve) is second
          |> List.sortBy (negate . Pair.second)
          |> List.map Pair.first
  let otherCurves = Maybe.collect nonIncidentCurve curveIncidences
  case incidentCurves of
    [] -> couldNotFindPointToFillet
    List.One{} -> couldNotFindPointToFillet
    List.ThreeOrMore{} -> couldNotFindPointToFillet
    List.Two firstCurve secondCurve -> Try.do
      firstTangent <- Curve2d.tangentDirection firstCurve
      secondTangent <- Curve2d.tangentDirection secondCurve
      let firstEndDirection = DirectionCurve2d.endValue firstTangent
      let secondStartDirection = DirectionCurve2d.startValue secondTangent
      let cornerAngle = Direction2d.angleFrom firstEndDirection secondStartDirection
      let offset = Qty.sign cornerAngle * Qty.abs radius
      let firstOffsetDisplacement =
            VectorCurve2d.rotateBy Angle.quarterTurn (offset * firstTangent)
      let secondOffsetDisplacement =
            VectorCurve2d.rotateBy Angle.quarterTurn (offset * secondTangent)
      let firstOffsetCurve = firstCurve + firstOffsetDisplacement
      let secondOffsetCurve = secondCurve + secondOffsetDisplacement
      maybeIntersections <- Curve2d.intersections firstOffsetCurve secondOffsetCurve
      case maybeIntersections of
        Nothing -> couldNotSolveForFilletLocation
        Just Curve2d.OverlappingSegments{} -> couldNotSolveForFilletLocation
        Just (Curve2d.IntersectionPoints intersectionPoints) -> do
          let firstParameterValue = Pair.first . IntersectionPoint.parameterValues
          let secondParameterValue = Pair.second . IntersectionPoint.parameterValues
          let intersection1 = NonEmpty.maximumBy firstParameterValue intersectionPoints
          let intersection2 = NonEmpty.minimumBy secondParameterValue intersectionPoints
          if intersection1 /= intersection2
            then couldNotSolveForFilletLocation
            else do
              let (t1, t2) = IntersectionPoint.parameterValues intersection1
              let centerPoint = Curve2d.evaluate firstOffsetCurve t1
              let startPoint = Curve2d.evaluate firstCurve t1
              let sweptAngle =
                    Direction2d.angleFrom
                      (DirectionCurve2d.evaluate firstTangent t1)
                      (DirectionCurve2d.evaluate secondTangent t2)
              let filletArc = Curve2d.sweptArc centerPoint startPoint sweptAngle
              let trimmedFirstCurve = firstCurve . Curve.line 0.0 t1
              let trimmedSecondCurve = secondCurve . Curve.line t2 1.0
              Success (filletArc : trimmedFirstCurve : trimmedSecondCurve : otherCurves)

curveIncidence ::
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  (Curve2d (space @ units), Maybe Float)
curveIncidence point curve
  | point ~= curve.startPoint = (curve, Just 0.0)
  | point ~= curve.endPoint = (curve, Just 1.0)
  | otherwise = (curve, Nothing)

incidentCurve :: (Curve2d (space @ units), Maybe Float) -> Maybe (Curve2d (space @ units), Float)
incidentCurve (curve, maybeIncidence) = Maybe.map (curve,) maybeIncidence

nonIncidentCurve :: (Curve2d (space @ units), Maybe Float) -> Maybe (Curve2d (space @ units))
nonIncidentCurve (curve, Nothing) = Just curve
nonIncidentCurve (_, Just _) = Nothing

-- checkForInnerIntersection ::
--   Tolerance units =>
--   List (Curve2d (space @ units)) ->
--   Result BoundedBy.Error ()
-- checkForInnerIntersection [] = Success ()
-- checkForInnerIntersection (first : rest) = Result.do
--   checkCurveForInnerIntersection first rest
--   checkForInnerIntersection rest

-- checkCurveForInnerIntersection ::
--   Tolerance units =>
--   Curve2d (space @ units) ->
--   List (Curve2d (space @ units)) ->
--   Result BoundedBy.Error ()
-- checkCurveForInnerIntersection _ [] = Success ()
-- checkCurveForInnerIntersection curve (first : rest) = Result.do
--   checkCurvesForInnerIntersection curve first
--   checkCurveForInnerIntersection curve rest

-- checkCurvesForInnerIntersection ::
--   Tolerance units =>
--   Curve2d (space @ units) ->
--   Curve2d (space @ units) ->
--   Result BoundedBy.Error ()
-- checkCurvesForInnerIntersection curve1 curve2 =
--   case Curve2d.intersections curve1 curve2 of
--     Failure Curve2d.Intersections.CurveHasDegeneracy ->
--       Failure BoundedBy.BoundaryCurveHasDegeneracy
--     -- We can ignore cases where either curve is actually a point,
--     -- since we'll still find any inner intersections
--     -- when we check with the *neighbours* of those degenerate curves
--     Failure Curve2d.Intersections.FirstCurveIsPoint -> Success ()
--     Failure Curve2d.Intersections.SecondCurveIsPoint -> Success ()
--     -- Any overlap between boundary curves is bad
--     Success (Just (Curve2d.OverlappingSegments _)) ->
--       Failure BoundedBy.BoundaryIntersectsItself
--     -- If there are no intersections at all then we're good!
--     Success Nothing -> Success ()
--     -- Otherwise, make sure curves only intersect (meet) at endpoints
--     Success (Just (Curve2d.IntersectionPoints intersectionPoints)) ->
--       if NonEmpty.allSatisfy isEndpointIntersection intersectionPoints
--         then Success ()
--         else Failure BoundedBy.BoundaryIntersectsItself

-- isEndpointIntersection :: IntersectionPoint -> Bool
-- isEndpointIntersection intersectionPoint = do
--   let (t1, t2) = Curve2d.IntersectionPoint.parameterValues intersectionPoint
--   Curve.isEndpoint t1 && Curve.isEndpoint t2

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
      let newCurve = if curve.endPoint ~= currentStart then curve else Curve2d.reverse curve
      let updatedCurves = NonEmpty.push newCurve currentCurves
      Success (PartialLoop newCurve.startPoint updatedCurves loopEnd, remaining)
    (List.TwoOrMore, _) -> Failure BoundedBy.BoundaryIntersectsItself

hasEndpoint :: Tolerance units => Point2d (space @ units) -> Curve2d (space @ units) -> Bool
hasEndpoint point curve = point ~= curve.startPoint || point ~= curve.endPoint

startLoop :: Curve2d (space @ units) -> PartialLoop (space @ units)
startLoop curve = PartialLoop curve.startPoint (NonEmpty.one curve) curve.endPoint

instance
  HasField
    "outerLoop"
    (Region2d (space @ units))
    (NonEmpty (Curve2d (space @ units)))
  where
  getField (Region2d loop _) = loop

instance
  HasField
    "innerLoops"
    (Region2d (space @ units))
    (List (NonEmpty (Curve2d (space @ units))))
  where
  getField (Region2d _ loops) = loops

instance
  HasField
    "boundaryLoops"
    (Region2d (space @ units))
    (NonEmpty (NonEmpty (Curve2d (space @ units))))
  where
  getField region = region.outerLoop :| region.innerLoops

instance
  HasField
    "boundaryCurves"
    (Region2d (space @ units))
    (NonEmpty (Curve2d (space @ units)))
  where
  getField region = NonEmpty.concat region.boundaryLoops

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Region2d (local @ units) ->
  Region2d (global @ units)
placeIn frame (Region2d outer inners) = do
  let transformLoop = NonEmpty.map (Curve2d.placeIn frame)
  Region2d (transformLoop outer) (List.map transformLoop inners)

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Region2d (global @ units) ->
  Region2d (local @ units)
relativeTo frame region = placeIn (Frame2d.inverse frame) region

transformBy ::
  Transform2d tag (space @ units) ->
  Region2d (space @ units) ->
  Region2d (space @ units)
transformBy transform (Region2d outer inners) = do
  let transformLoop =
        case Transform2d.handedness transform of
          Positive -> NonEmpty.map (Curve2d.transformBy transform)
          Negative -> NonEmpty.reverseMap (Curve2d.transformBy transform >> Curve2d.reverse)
  Region2d (transformLoop outer) (List.map transformLoop inners)

translateBy ::
  Vector2d (space @ units) ->
  Region2d (space @ units) ->
  Region2d (space @ units)
translateBy = Transform2d.translateByImpl transformBy

translateIn ::
  Direction2d space ->
  Qty units ->
  Region2d (space @ units) ->
  Region2d (space @ units)
translateIn = Transform2d.translateInImpl transformBy

translateAlong ::
  Axis2d (space @ units) ->
  Qty units ->
  Region2d (space @ units) ->
  Region2d (space @ units)
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround ::
  Point2d (space @ units) ->
  Angle ->
  Region2d (space @ units) ->
  Region2d (space @ units)
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross ::
  Axis2d (space @ units) ->
  Region2d (space @ units) ->
  Region2d (space @ units)
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

scaleAbout ::
  Point2d (space @ units) ->
  Float ->
  Region2d (space @ units) ->
  Region2d (space @ units)
scaleAbout = Transform2d.scaleAboutImpl transformBy

scaleAlong ::
  Axis2d (space @ units) ->
  Float ->
  Region2d (space @ units) ->
  Region2d (space @ units)
scaleAlong = Transform2d.scaleAlongImpl transformBy

convert ::
  Qty (units2 :/: units1) ->
  Region2d (space @ units1) ->
  Region2d (space @ units2)
convert factor (Region2d outer inners) = do
  let transform =
        case Qty.sign factor of
          Positive -> NonEmpty.map (Curve2d.convert factor)
          Negative -> NonEmpty.reverseMap (Curve2d.reverse . Curve2d.convert factor)
  Region2d (transform outer) (List.map transform inners)

unconvert ::
  Qty (units2 :/: units1) ->
  Region2d (space @ units2) ->
  Region2d (space @ units1)
unconvert factor region = convert (Units.simplify (1.0 ./. factor)) region

contains :: Tolerance units => Point2d (space @ units) -> Region2d (space @ units) -> Bool
contains point region =
  case classify point region.boundaryCurves of
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
  Tolerance units =>
  Point2d (space @ units) ->
  Curve2d (space @ units) ->
  Estimate Unitless
fluxIntegral point curve = do
  let displacement = point - curve
  -- By this point we've already checked whether the poing is *on* the curve,
  -- so the Curve.unsafeQuotient call should be OK
  -- (if the point is not on the curve, then the displacement will always be non-zero)
  let integrand =
        Tolerance.using Tolerance.squared' $
          Curve.unsafeQuotient
            @ curve.derivative `cross'` displacement
            @ VectorCurve2d.squaredMagnitude' displacement
  Curve.integrate integrand

totalFlux :: Tolerance units => Point2d (space @ units) -> Loop (space @ units) -> Estimate Unitless
totalFlux point loop = Estimate.sum (NonEmpty.map (fluxIntegral point) loop)

classifyNonBoundary :: Tolerance units => Point2d (space @ units) -> Loop (space @ units) -> Sign
classifyNonBoundary point loop = do
  let flux = Estimate.satisfy containmentIsDeterminate (totalFlux point loop)
  if Bounds.includes Qty.zero flux then Negative else Positive

bothPossibleFluxValues :: Bounds Unitless
bothPossibleFluxValues = Bounds 0.0 Float.twoPi

containmentIsDeterminate :: Bounds Unitless -> Bool
containmentIsDeterminate flux = not (Bounds.contains bothPossibleFluxValues flux)

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
  let referencePoint = loop.first.startPoint
  let edgeIntegrals = NonEmpty.map (areaIntegral' referencePoint) loop
  Estimate.sum edgeIntegrals

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
  let y = displacement.yComponent
  let dx = displacement.xComponent.derivative
  -(Curve.integrate (y .*. dx))

loopIsInside :: Tolerance units => Loop (space @ units) -> Loop (space @ units) -> Bool
loopIsInside outer inner = do
  let testPoint = inner.first.startPoint
  case classify testPoint outer of
    Nothing -> True -- Shouldn't happen, loops should be guaranteed not to be touching by this point
    Just Positive -> True
    Just Negative -> False

bounds :: Region2d (space @ units) -> Bounds2d (space @ units)
bounds region = do
  let outerEdgeBoundingBoxes = NonEmpty.map Curve2d.bounds region.outerLoop
  NonEmpty.reduce Bounds2d.aggregate2 outerEdgeBoundingBoxes

area :: Units.Squared units1 units2 => Region2d (space @ units1) -> Estimate units2
area region = do
  let referencePoint = region.outerLoop.first.startPoint
  let edgeIntegrals = NonEmpty.map (areaIntegral referencePoint) region.boundaryCurves
  Estimate.sum edgeIntegrals

toMesh :: Resolution units -> Region2d (space @ units) -> Mesh (Point2d (space @ units))
toMesh resolution region = do
  let allLoops = region.outerLoop :| region.innerLoops
  let vertexLoops = NonEmpty.map (toVertexLoop resolution) allLoops
  CDT.unsafe vertexLoops []

toVertexLoop ::
  Resolution units ->
  NonEmpty (Curve2d (space @ units)) ->
  NonEmpty (Point2d (space @ units))
toVertexLoop resolution loop = do
  let trailingVertices curve = (Curve2d.toPolyline resolution curve).vertices.rest
  let allVertices = List.collect trailingVertices loop
  case allVertices of
    NonEmpty vertices -> vertices
    [] -> internalError "Should always have at least one vertex"
