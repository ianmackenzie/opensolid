module OpenSolid.Region2D
  ( Region2D
  , EmptyRegion (EmptyRegion)
  , boundedBy
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
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.CDT qualified as CDT
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Curve.IntersectionPoint qualified as Curve.IntersectionPoint
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import OpenSolid.Estimate (Estimate)
import OpenSolid.Estimate qualified as Estimate
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Line2D (pattern Line2D)
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Mesh (Mesh)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (IsDegenerate (IsDegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Polygon2D (Polygon2D)
import OpenSolid.Polygon2D qualified as Polygon2D
import OpenSolid.Polyline2D qualified as Polyline2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D.BoundedBy qualified as BoundedBy
import OpenSolid.Resolution (Resolution)
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

type role Region2D nominal

-- | A closed 2D region (possibly with holes), defined by a set of boundary curves.
data Region2D units = Region2D
  { outerLoop :: Loop units
  , innerLoops :: List (Loop units)
  }

type Loop units = NonEmpty (Curve2D units)

instance FFI (Region2D Meters) where
  representation = FFI.classRepresentation "Region2D"

instance FFI (Region2D Unitless) where
  representation = FFI.classRepresentation "UvRegion"

instance HasUnits (Region2D units) units

instance Units.Coercion (Region2D units1) (Region2D units2) where
  coerce (Region2D outer inners) =
    Region2D
      (NonEmpty.map Units.coerce outer)
      (List.map (NonEmpty.map Units.coerce) inners)

{-| Create a region bounded by the given curves.

The curves may be given in any order,
do not need to have consistent directions
and can form multiple separate loops if the region has holes.
However, the curves must not overlap or intersect (other than at endpoints)
and there must not be any gaps between them.
-}
boundedBy :: Tolerance units => List (Curve2D units) -> Result BoundedBy.Error (Region2D units)
boundedBy curves = do
  checkForInnerIntersection curves
  loops <- connect curves
  classifyLoops loops

data EmptyRegion = EmptyRegion deriving (Eq, Show)

{-| Create a rectangular region.

Fails if the given bounds are empty
(zero area, i.e. zero width in either direction).
-}
rectangle :: Tolerance units => Bounds2D units -> Result EmptyRegion (Region2D units)
rectangle (Bounds2D xBounds yBounds) =
  if Interval.width xBounds ~= Quantity.zero || Interval.width yBounds ~= Quantity.zero
    then Error EmptyRegion
    else Ok do
      let Interval x1 x2 = xBounds
      let Interval y1 y2 = yBounds
      let p11 = Point2D x1 y1
      let p12 = Point2D x1 y2
      let p21 = Point2D x2 y1
      let p22 = Point2D x2 y2
      let edges =
            NonEmpty.four
              (Curve2D.lineFrom p11 p21)
              (Curve2D.lineFrom p21 p22)
              (Curve2D.lineFrom p22 p12)
              (Curve2D.lineFrom p12 p11)
      Region2D edges []

-- | Create a region from the given circle.
circle :: Tolerance units => Circle2D units -> Result EmptyRegion (Region2D units)
circle givenCircle =
  if Circle2D.diameter givenCircle ~= Quantity.zero
    then Error EmptyRegion
    else Ok (Region2D (NonEmpty.one (Curve2D.circle givenCircle)) [])

-- | Create a region from the given polygon.
polygon :: Tolerance units => Polygon2D units -> Result BoundedBy.Error (Region2D units)
polygon givenPolygon = do
  let toCurve (Line2D p1 p2) = if p1 ~= p2 then Nothing else Just (Curve2D.lineFrom p1 p2)
  boundedBy (NonEmpty.filterMap toCurve (Polygon2D.edges givenPolygon))

{-| Fillet a region at the given corner points, with the given radius.

Fails if any of the given points are not actually corner points of the region
(within the given tolerance),
or if it is not possible to solve for a given fillet
(e.g. if either of the adjacent edges is not long enough).
-}
fillet ::
  Tolerance units =>
  List (Point2D units) ->
  "radius" ::: Quantity units ->
  Region2D units ->
  Result Text (Region2D units)
fillet points ("radius" ::: radius) region = do
  let initialCurves = NonEmpty.toList (boundaryCurves region)
  filletedCurves <- Result.orFail (Result.foldl (addFillet radius) initialCurves points)
  Result.orFail (boundedBy filletedCurves)

addFillet ::
  Tolerance units =>
  Quantity units ->
  List (Curve2D units) ->
  Point2D units ->
  Result Text (List (Curve2D units))
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
          & List.sortBy (negate . Pair.second)
          & List.map Pair.first
  let otherCurves = List.filterMap nonIncidentCurve curveIncidences
  case incidentCurves of
    [] -> couldNotFindPointToFillet
    List.One _ -> couldNotFindPointToFillet
    List.ThreeOrMore -> couldNotFindPointToFillet
    List.Two firstCurve secondCurve -> do
      firstTangent <- Result.orFail (Curve2D.tangentDirection firstCurve)
      secondTangent <- Result.orFail (Curve2D.tangentDirection secondCurve)
      let firstEndDirection = DirectionCurve2D.endValue firstTangent
      let secondStartDirection = DirectionCurve2D.startValue secondTangent
      let cornerAngle = Direction2D.angleFrom firstEndDirection secondStartDirection
      let offset = Quantity.sign cornerAngle * Quantity.abs radius
      firstOffsetCurve <- Result.orFail (Curve2D.offsetLeftwardBy offset firstCurve)
      secondOffsetCurve <- Result.orFail (Curve2D.offsetLeftwardBy offset secondCurve)
      maybeIntersections <- Result.orFail (Curve2D.intersections firstOffsetCurve secondOffsetCurve)
      case maybeIntersections of
        Nothing -> couldNotSolveForFilletLocation
        Just Curve.OverlappingSegments{} -> couldNotSolveForFilletLocation
        Just (Curve.IntersectionPoints intersectionPoints) -> do
          let intersection1 =
                intersectionPoints
                  & NonEmpty.maximumBy (Pair.first . Curve.IntersectionPoint.parameterValues)
          let intersection2 =
                intersectionPoints
                  & NonEmpty.minimumBy (Pair.second . Curve.IntersectionPoint.parameterValues)
          if intersection1 /= intersection2
            then couldNotSolveForFilletLocation
            else do
              let (t1, t2) = Curve.IntersectionPoint.parameterValues intersection1
              let centerPoint = Curve2D.point firstOffsetCurve t1
              let startPoint = Curve2D.point firstCurve t1
              let sweptAngle =
                    Direction2D.angleFrom
                      (DirectionCurve2D.value firstTangent t1)
                      (DirectionCurve2D.value secondTangent t2)
              let filletArc = Curve2D.sweptArc centerPoint startPoint sweptAngle
              let trimmedFirstCurve = firstCurve . Curve1D.interpolateFrom 0.0 t1
              let trimmedSecondCurve = secondCurve . Curve1D.interpolateFrom t2 1.0
              Ok (filletArc : trimmedFirstCurve : trimmedSecondCurve : otherCurves)

curveIncidence :: Tolerance units => Point2D units -> Curve2D units -> (Curve2D units, Maybe Number)
curveIncidence point curve
  | point ~= Curve2D.startPoint curve = (curve, Just 0.0)
  | point ~= Curve2D.endPoint curve = (curve, Just 1.0)
  | otherwise = (curve, Nothing)

incidentCurve :: (Curve2D units, Maybe Number) -> Maybe (Curve2D units, Number)
incidentCurve (curve, maybeIncidence) = Maybe.map (curve,) maybeIncidence

nonIncidentCurve :: (Curve2D units, Maybe Number) -> Maybe (Curve2D units)
nonIncidentCurve (curve, Nothing) = Just curve
nonIncidentCurve (_, Just _) = Nothing

checkForInnerIntersection :: Tolerance units => List (Curve2D units) -> Result BoundedBy.Error ()
checkForInnerIntersection [] = Ok ()
checkForInnerIntersection (first : rest) = do
  checkCurveForInnerIntersection first rest
  checkForInnerIntersection rest

checkCurveForInnerIntersection ::
  Tolerance units =>
  Curve2D units ->
  List (Curve2D units) ->
  Result BoundedBy.Error ()
checkCurveForInnerIntersection _ [] = Ok ()
checkCurveForInnerIntersection curve (first : rest) = do
  checkCurvesForInnerIntersection curve first
  checkCurveForInnerIntersection curve rest

checkCurvesForInnerIntersection ::
  Tolerance units =>
  Curve2D units ->
  Curve2D units ->
  Result BoundedBy.Error ()
checkCurvesForInnerIntersection curve1 curve2 =
  case Curve2D.intersections curve1 curve2 of
    -- We can ignore cases where either curve is actually a point,
    -- since we'll still find any inner intersections
    -- when we check with the *neighbours* of those degenerate curves
    Error IsDegenerate -> Ok ()
    -- Any overlap between boundary curves is bad
    Ok (Just Curve.OverlappingSegments{}) ->
      Error BoundedBy.BoundaryIntersectsItself
    -- If there are no intersections at all then we're good!
    Ok Nothing -> Ok ()
    -- Otherwise, make sure curves only intersect (meet) at endpoints
    Ok (Just (Curve.IntersectionPoints intersectionPoints)) ->
      if NonEmpty.all isEndpointIntersection intersectionPoints
        then Ok ()
        else Error BoundedBy.BoundaryIntersectsItself

isEndpointIntersection :: IntersectionPoint -> Bool
isEndpointIntersection intersectionPoint = do
  let (t1, t2) = Curve.IntersectionPoint.parameterValues intersectionPoint
  Parameter.isEndpoint t1 && Parameter.isEndpoint t2

connect :: Tolerance units => List (Curve2D units) -> Result BoundedBy.Error (List (Loop units))
connect [] = Ok []
connect (first : rest) = do
  (loop, remainingCurves) <- buildLoop (startLoop first) rest
  remainingLoops <- connect remainingCurves
  Ok (loop : remainingLoops)

data PartialLoop units
  = PartialLoop (Point2D units) (NonEmpty (Curve2D units)) (Point2D units)

buildLoop ::
  Tolerance units =>
  PartialLoop units ->
  List (Curve2D units) ->
  Result BoundedBy.Error (Loop units, List (Curve2D units))
buildLoop partialLoop@(PartialLoop currentStart currentCurves loopEnd) remainingCurves
  | currentStart ~= loopEnd = Ok (currentCurves, remainingCurves)
  | otherwise = do
      (updatedPartialLoop, updatedRemainingCurves) <- extendPartialLoop partialLoop remainingCurves
      buildLoop updatedPartialLoop updatedRemainingCurves

extendPartialLoop ::
  Tolerance units =>
  PartialLoop units ->
  List (Curve2D units) ->
  Result BoundedBy.Error (PartialLoop units, List (Curve2D units))
extendPartialLoop (PartialLoop currentStart currentCurves loopEnd) curves =
  case List.partition (hasEndpoint currentStart) curves of
    ([], _) -> Error BoundedBy.BoundaryHasGaps
    (List.One curve, remaining) -> do
      let newCurve = if Curve2D.endPoint curve ~= currentStart then curve else Curve2D.reverse curve
      let updatedCurves = NonEmpty.push newCurve currentCurves
      Ok (PartialLoop (Curve2D.startPoint newCurve) updatedCurves loopEnd, remaining)
    (List.TwoOrMore, _) -> Error BoundedBy.BoundaryIntersectsItself

hasEndpoint :: Tolerance units => Point2D units -> Curve2D units -> Bool
hasEndpoint point curve = point ~= Curve2D.startPoint curve || point ~= Curve2D.endPoint curve

startLoop :: Curve2D units -> PartialLoop units
startLoop curve =
  PartialLoop (Curve2D.startPoint curve) (NonEmpty.one curve) (Curve2D.endPoint curve)

{-| The list of curves forming the outer boundary of the region.

The curves will be in counterclockwise order around the region,
and each curve will be in the counterclockwise direction.
-}
outerLoop :: Region2D units -> NonEmpty (Curve2D units)
outerLoop = (.outerLoop)

{-| The lists of curves (if any) forming the holes within the region.

The curves will be in clockwise order around each hole,
and each curve will be in the clockwise direction.
-}
innerLoops :: Region2D units -> List (NonEmpty (Curve2D units))
innerLoops = (.innerLoops)

boundaryLoops :: Region2D units -> NonEmpty (NonEmpty (Curve2D units))
boundaryLoops region = outerLoop region :| innerLoops region

-- | The list of all (outer and inner) boundary curves of a region.
boundaryCurves :: Region2D units -> NonEmpty (Curve2D units)
boundaryCurves region = NonEmpty.concat (boundaryLoops region)

placeIn :: Frame2D units -> Region2D units -> Region2D units
placeIn frame (Region2D outer inners) = do
  let transformLoop = NonEmpty.map (Curve2D.placeIn frame)
  Region2D (transformLoop outer) (List.map transformLoop inners)

relativeTo :: Frame2D units -> Region2D units -> Region2D units
relativeTo frame region = placeIn (Frame2D.inverse frame) region

transformBy :: Transform2D tag units -> Region2D units -> Region2D units
transformBy transform (Region2D outer inners) = do
  let transformLoop =
        case Transform2D.handedness transform of
          Positive -> NonEmpty.map (Curve2D.transformBy transform)
          Negative -> NonEmpty.reverseMap (Curve2D.reverse . Curve2D.transformBy transform)
  Region2D (transformLoop outer) (List.map transformLoop inners)

translateBy :: Vector2D units -> Region2D units -> Region2D units
translateBy = Transform2D.translateByImpl transformBy

translateIn :: Direction2D -> Quantity units -> Region2D units -> Region2D units
translateIn = Transform2D.translateInImpl transformBy

translateAlong :: Axis2D units -> Quantity units -> Region2D units -> Region2D units
translateAlong = Transform2D.translateAlongImpl transformBy

rotateAround :: Point2D units -> Angle -> Region2D units -> Region2D units
rotateAround = Transform2D.rotateAroundImpl transformBy

mirrorAcross :: Axis2D units -> Region2D units -> Region2D units
mirrorAcross = Transform2D.mirrorAcrossImpl transformBy

scaleAbout :: Point2D units -> Number -> Region2D units -> Region2D units
scaleAbout = Transform2D.scaleAboutImpl transformBy

scaleAlong :: Axis2D units -> Number -> Region2D units -> Region2D units
scaleAlong = Transform2D.scaleAlongImpl transformBy

convert :: Quantity (units2 ?/? units1) -> Region2D units1 -> Region2D units2
convert factor (Region2D outer inners) = do
  let transform =
        case Quantity.sign factor of
          Positive -> NonEmpty.map (Curve2D.convert factor)
          Negative -> NonEmpty.reverseMap (Curve2D.reverse . Curve2D.convert factor)
  Region2D (transform outer) (List.map transform inners)

unconvert :: Quantity (units2 ?/? units1) -> Region2D units2 -> Region2D units1
unconvert factor region = convert (Units.simplify (1.0 ?/? factor)) region

contains :: Tolerance units => Point2D units -> Region2D units -> Bool
contains point region =
  case classify point (boundaryCurves region) of
    Nothing -> True -- Point on boundary is considered contained
    Just Positive -> True
    Just Negative -> False

classify :: Tolerance units => Point2D units -> NonEmpty (Curve2D units) -> Maybe Sign
classify point curves =
  if NonEmpty.any (intersects point) curves
    then Nothing
    else Just (classifyNonBoundary point curves)

fluxIntegral :: Tolerance units => Point2D units -> Curve2D units -> Estimate Unitless
fluxIntegral point curve = do
  let displacement = point - curve
  -- By this point we've already checked whether the point is *on* the curve,
  -- so (since it's not) the displacement will always be non-zero
  let integrand =
        (Curve2D.derivative curve `cross_` displacement)
          / Nonzero (VectorCurve2D.squaredMagnitude_ displacement)
  Curve1D.integrate integrand

totalFlux :: Tolerance units => Point2D units -> Loop units -> Estimate Unitless
totalFlux point loop = Estimate.sum (NonEmpty.map (fluxIntegral point) loop)

classifyNonBoundary :: Tolerance units => Point2D units -> Loop units -> Sign
classifyNonBoundary point loop = do
  let flux = Estimate.satisfy containmentIsDeterminate (totalFlux point loop)
  if Interval.includes Quantity.zero flux then Negative else Positive

bothPossibleFluxValues :: Interval Unitless
bothPossibleFluxValues = Interval 0.0 Number.twoPi

containmentIsDeterminate :: Interval Unitless -> Bool
containmentIsDeterminate flux = not (Interval.contains bothPossibleFluxValues flux)

classifyLoops :: Tolerance units => List (Loop units) -> Result BoundedBy.Error (Region2D units)
classifyLoops [] = Error BoundedBy.EmptyRegion
classifyLoops (NonEmpty loops) = do
  let (largestLoop, smallerLoops) = pickLargestLoop loops
  let outerLoopCandidate = fixSign Positive largestLoop
  let innerLoopCandidates = List.map (fixSign Negative) smallerLoops
  if List.all (loopIsInside outerLoopCandidate) innerLoopCandidates
    then Ok (Region2D outerLoopCandidate innerLoopCandidates)
    else Error BoundedBy.MultipleDisjointRegions

fixSign :: Tolerance units => Sign -> Loop units -> Loop units
fixSign desiredSign loop =
  Tolerance.using (Quantity.squared_ ?tolerance) do
    if Estimate.sign (loopSignedArea_ loop) == desiredSign then loop else reverseLoop loop

reverseLoop :: Loop units -> Loop units
reverseLoop loop = NonEmpty.reverseMap Curve2D.reverse loop

pickLargestLoop :: Tolerance units => NonEmpty (Loop units) -> (Loop units, List (Loop units))
pickLargestLoop loops =
  Tolerance.using (Quantity.squared_ ?tolerance) do
    Estimate.pickMaximumBy (Estimate.abs . loopSignedArea_) loops

loopSignedArea_ :: Loop units -> Estimate (units ?*? units)
loopSignedArea_ loop = do
  let referencePoint = Curve2D.startPoint (NonEmpty.first loop)
  let edgeIntegrals = NonEmpty.map (areaIntegral_ referencePoint) loop
  Estimate.sum edgeIntegrals

areaIntegral :: Units.Squared units1 units2 => Point2D units1 -> Curve2D units1 -> Estimate units2
areaIntegral referencePoint curve =
  Units.specialize (areaIntegral_ referencePoint curve)

areaIntegral_ :: Point2D units -> Curve2D units -> Estimate (units ?*? units)
areaIntegral_ referencePoint curve = do
  let (x, y) = VectorCurve2D.components (curve - referencePoint)
  negate (Curve1D.integrate (y ?*? Curve1D.derivative x))

loopIsInside :: Tolerance units => Loop units -> Loop units -> Bool
loopIsInside outer inner = do
  let testPoint = Curve2D.startPoint (NonEmpty.first inner)
  case classify testPoint outer of
    Nothing -> True -- Shouldn't happen, loops should be guaranteed not to be touching by this point
    Just Positive -> True
    Just Negative -> False

bounds :: Region2D units -> Bounds2D units
bounds region = Bounds2D.aggregateOf Curve2D.overallBounds (outerLoop region)

area :: Units.Squared units1 units2 => Region2D units1 -> Estimate units2
area region = do
  let referencePoint = Curve2D.startPoint (NonEmpty.first (outerLoop region))
  let edgeIntegrals = NonEmpty.map (areaIntegral referencePoint) (boundaryCurves region)
  Estimate.sum edgeIntegrals

toMesh :: Resolution units -> Region2D units -> Mesh (Point2D units)
toMesh resolution region = do
  let vertexLoops = NonEmpty.map (toVertexLoop resolution) (boundaryLoops region)
  CDT.unsafe vertexLoops []

toVertexLoop :: Resolution units -> NonEmpty (Curve2D units) -> NonEmpty (Point2D units)
toVertexLoop resolution loop = do
  let trailingVertices curve = do
        let polyline = Curve2D.toPolyline resolution curve
        NonEmpty.rest (Polyline2D.vertices polyline)
  let allVertices = List.combine trailingVertices (NonEmpty.toList loop)
  case allVertices of
    NonEmpty vertices -> vertices
    [] -> throw (InternalError "Should always have at least one vertex")
