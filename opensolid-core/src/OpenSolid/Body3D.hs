module OpenSolid.Body3D
  ( Body3D
  , block
  , sphere
  , cylinder
  , cylinderAlong
  , extruded
  , sweptBy
  , revolved
  , boundedBy
  , toPointMesh
  , toSurfaceMesh
  , surfaces
  , placeIn
  , relativeTo
  )
where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Axis3D (Axis3D (Axis3D))
import OpenSolid.Axis3D qualified as Axis3D
import OpenSolid.Bag3D qualified as Bag3D
import OpenSolid.Body3D.BoundedBy qualified as BoundedBy
import OpenSolid.Body3D.HalfEdge (HalfEdge (..))
import OpenSolid.Body3D.HalfEdge qualified as HalfEdge
import OpenSolid.Body3D.Ids (BoundaryId (BoundaryId), CurveId (CurveId), SurfaceId (SurfaceId))
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CDT qualified as CDT
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.IsZero (IsZero (IsZero))
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Line2D (Line2D)
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Polygon2D (Polygon2D (Polygon2D))
import OpenSolid.Polygon2D qualified as Polygon2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution (Resolution)
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set2D qualified as Set2D
import OpenSolid.Set3D (Set3D)
import OpenSolid.Set3D qualified as Set3D
import OpenSolid.Space qualified as Space
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.SurfaceVertex3D (SurfaceVertex3D)
import OpenSolid.UvBounds (UvBounds, data UvBounds)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D
import OpenSolid.World3D qualified as World3D

-- | A solid body in 3D, defined by a set of boundary surfaces.
data Body3D space = Body3D
  { surfaces :: Set3D space (Surface3D space)
  , seams :: HashMap HalfEdge.Id (Maybe HalfEdge.Id)
  }

instance Indexed (Body3D space) SurfaceId (Surface3D space) where
  body @ surfaceId = body.surfaces @ surfaceId

instance Indexed (Body3D space) HalfEdge.Id (SurfaceCurve3D space) where
  body @ HalfEdge.Id{surfaceId, boundaryId, curveId} = body @ surfaceId @ boundaryId @ curveId

instance FFI (Body3D Void) where
  representation = FFI.classRepresentation "Body3D"

instance Space.Coercion (Body3D space1) (Body3D space2) where
  coerce body =
    Body3D
      { surfaces = Set3D.map Space.coerce body.surfaces
      , seams = body.seams
      }

----- CONSTRUCTION -----

data EmptyBody = EmptyBody deriving (Eq, Show, Err)

{-| Create a rectangular block body.

Fails if the given bounds are empty (the length, width, or height is zero).
-}
block :: Tolerance Meters => Bounds3D space -> Result EmptyBody (Body3D space)
block bounds =
  case Region2D.rectangle (Bounds3D.projectInto World3D.topPlane bounds) of
    Err Region2D.EmptyRegion -> Err EmptyBody
    Ok profile -> do
      let Interval h1 h2 = Bounds3D.upwardCoordinate bounds
      if h1 ~= h2
        then Err EmptyBody
        else case extruded World3D.topPlane profile h1 h2 of
          Ok body -> Ok body
          Err _ -> error "Constructing block body from non-empty bounds should not fail"

{-| Create a sphere with the given center point and diameter.

Fails if the given diameter is zero.
-}
sphere ::
  Tolerance Meters =>
  "centerPoint" ::: Point3D space ->
  "diameter" ::: Length ->
  Result EmptyBody (Body3D space)
sphere ("centerPoint" ::: centerPoint) ("diameter" ::: diameter)
  | diameter ~= Length.zero = Err EmptyBody
  | otherwise = Ok do
      let panic = error "Constructing sphere from non-zero diameter should not fail"
      let r = 0.5 * diameter
      let arc = Curve2D.arcFrom (Point2D.y r) (Point2D.y -r) -Angle.pi ! panic
      let plane = World3D.forwardPlane centerPoint
      let revolvedSurface = Surface3D.revolved plane arc Axis2D.y Angle.twoPi ! panic
      boundedBy [revolvedSurface] ! panic

{-| Create a cylindrical body from a start point, end point and diameter.

Fails if the cylinder length or diameter is zero.
-}
cylinder ::
  Tolerance Meters =>
  Point3D space ->
  Point3D space ->
  "diameter" ::: Length ->
  Result EmptyBody (Body3D space)
cylinder startPoint endPoint ("diameter" ::: diameter) =
  case Vector3D.magnitudeAndDirection (endPoint - startPoint) of
    Err IsZero -> Err EmptyBody
    Ok (length, direction) ->
      cylinderAlong (Axis3D startPoint direction) Length.zero length (#diameter diameter)

{-| Create a cylindrical body along a given axis.

In addition to the axis itself, you will need to provide:

- Where along the axis the cylinder starts and ends
  (given as a range of distances along the axis).
- The cylinder diameter.

Failes if the cylinder length or diameter is zero.
-}
cylinderAlong ::
  Tolerance Meters =>
  Axis3D space ->
  Length ->
  Length ->
  "diameter" ::: Length ->
  Result EmptyBody (Body3D space)
cylinderAlong axis d1 d2 ("diameter" ::: diameter) =
  case Region2D.circle (Circle2D.withDiameter diameter Point2D.origin) of
    Err Region2D.EmptyRegion -> Err EmptyBody
    Ok profile ->
      if d1 ~= d2
        then Err EmptyBody
        else case extruded (Axis3D.normalPlane axis) profile d1 d2 of
          Ok body -> Ok body
          Err _ -> error "Constructing non-empty cylinder body should not fail"

-- | Create an extruded body from a sketch plane and profile.
extruded ::
  Tolerance Meters =>
  Plane3D space ->
  Region2D Meters ->
  Length ->
  Length ->
  Result BoundedBy.Error (Body3D space)
extruded sketchPlane profile d1 d2 = do
  let normal = Plane3D.normalDirection sketchPlane
  let v1 = d1 * normal
  let v2 = d2 * normal
  sweptBy (VectorCurve3D.interpolateFrom v1 v2) sketchPlane profile

sweptBy ::
  Tolerance Meters =>
  VectorCurve3D Meters space ->
  Plane3D space ->
  Region2D Meters ->
  Result BoundedBy.Error (Body3D space)
sweptBy givenDisplacementCurve sketchPlane profile = do
  -- Fix displacement curve so that extrusion is upwards from plane
  let givenStartDerivative = VectorCurve3D.startDerivative givenDisplacementCurve
  let displacementCurve =
        case Quantity.sign (givenStartDerivative `dot` Plane3D.normalDirection sketchPlane) of
          Positive -> givenDisplacementCurve
          Negative -> VectorCurve3D.reverse givenDisplacementCurve
  let startPlane = Plane3D.translateBy (VectorCurve3D.startValue displacementCurve) sketchPlane
  let endPlane = Plane3D.translateBy (VectorCurve3D.endValue displacementCurve) sketchPlane
  startCap <- Surface3D.on startPlane profile & Result.map Surface3D.flip !! Err BoundedBy.EmptyBody
  endCap <- Surface3D.on endPlane profile !! Err BoundedBy.EmptyBody
  let profileCurves = Set2D.toList (Region2D.boundaryCurves profile)
  let sideSurface curve = Surface3D.sweptBy displacementCurve (Curve2D.placeOn sketchPlane curve)
  sideSurfaces <- Result.collect sideSurface profileCurves !! Err BoundedBy.EmptyBody
  boundedBy (startCap : endCap : sideSurfaces)

{-| Create a revolved body from a sketch plane and profile.

Note that the revolution profile and revolution axis
are both defined within the given sketch plane.

A positive angle will result in a counterclockwise revolution around the axis,
and a negative angle will result in a clockwise revolution.
-}
revolved ::
  Tolerance Meters =>
  Plane3D space ->
  Region2D Meters ->
  Axis2D Meters ->
  Angle ->
  Result BoundedBy.Error (Body3D space)
revolved sketchPlane profile givenAxis givenSweptAngle = do
  let profileCurves = Set2D.toNonEmpty (Region2D.boundaryCurves profile)
  let offAxisCurves = NonEmpty.filter (not . Curve2D.isOnAxis givenAxis) profileCurves
  let signedDistanceCurves = List.map (Curve2D.distanceLeftOf givenAxis) offAxisCurves
  -- Check if the given profile is to the left of the given axis ('positive')
  -- or to the right ('negative')
  profileSign <-
    case Result.collect Curve1D.sign signedDistanceCurves of
      Err Curve1D.CrossesZero -> Err BoundedBy.BoundaryIntersectsItself
      Ok curveSigns
        | List.all (== Positive) curveSigns -> Ok Positive
        | List.all (== Negative) curveSigns -> Ok Negative
        | otherwise -> Err BoundedBy.BoundaryIntersectsItself
  let planeRotationAxis = Axis2D.placeOn sketchPlane givenAxis
  let rotatedPlane = Plane3D.rotateAround planeRotationAxis givenSweptAngle sketchPlane
  let (startPlane, endPlane) =
        case profileSign * Quantity.sign givenSweptAngle of
          Positive -> (sketchPlane, rotatedPlane)
          Negative -> (rotatedPlane, sketchPlane)
  let sweptAngle = Quantity.abs givenSweptAngle
  startCap <- Surface3D.on startPlane profile & Result.map Surface3D.flip !! Err BoundedBy.EmptyBody
  endCap <- Surface3D.on endPlane profile !! Err BoundedBy.EmptyBody
  let isFullRevolution = angular (sweptAngle ~= Angle.twoPi)
  let endSurfaces = if isFullRevolution then [] else [startCap, endCap]
  -- A 2D axis such that the profile is to the *left* of the axis
  -- (such that it comes "out of the page" when revolved,
  -- in turn meaning that the side surfaces have the correct normal orientation)
  let axis2D = profileSign * givenAxis
  let sideSurface profileCurve = Surface3D.revolved startPlane profileCurve axis2D sweptAngle
  sideSurfaces <- Result.collect sideSurface offAxisCurves !! Err BoundedBy.EmptyBody
  boundedBy (endSurfaces <> sideSurfaces)

{-| Create a body bounded by the given surfaces.
The surfaces do not have to have consistent orientation,
but currently the *first* surface must have the correct orientation
since all others will be flipped if necessary to match it.
-}
boundedBy :: Tolerance Meters => List (Surface3D space) -> Result BoundedBy.Error (Body3D space)
boundedBy [] = Err BoundedBy.EmptyBody
boundedBy (NonEmpty givenSurfaces) = do
  let surfaceSet = Set3D.build givenSurfaces
  let halfEdgeSet = buildHalfEdgeSet surfaceSet
  seams <- HashMap.empty & Result.forEach halfEdgeSet (registerSeam halfEdgeSet)
  Ok Body3D{surfaces = surfaceSet, seams}

buildHalfEdgeSet ::
  Tolerance Meters =>
  Set3D space (Surface3D space) ->
  Set3D space (HalfEdge space)
buildHalfEdgeSet surfaceSet =
  surfaceSet & Set3D.combineWithIndex \surfaceIndex surface -> do
    let surfaceId = SurfaceId surfaceIndex
    let surfaceBoundaries = Surface3D.boundaries surface
    surfaceBoundaries & Set3D.combineWithIndex \boundaryIndex boundary -> do
      let boundaryId = BoundaryId boundaryIndex
      boundary & Set3D.combineWithIndex \curveIndex surfaceCurve -> do
        let curveId = CurveId curveIndex
        let halfEdgeId = HalfEdge.Id{surfaceId, boundaryId, curveId}
        let halfEdge = HalfEdge{id = halfEdgeId, surfaceCurve}
        Set3D.leaf halfEdge

registerSeam ::
  Tolerance Meters =>
  Set3D space (HalfEdge space) ->
  HalfEdge space ->
  HashMap HalfEdge.Id (Maybe HalfEdge.Id) ->
  Result BoundedBy.Error (HashMap HalfEdge.Id (Maybe HalfEdge.Id))
registerSeam halfEdgeSet halfEdge accumulated =
  case accumulated & HashMap.lookup halfEdge.id of
    Just _ -> Ok accumulated -- We've already registered this seam from the other side
    Nothing ->
      case HalfEdge.surfaceCurve halfEdge of
        SurfaceCurve3D.Pole _ ->
          Ok (accumulated & HashMap.insert halfEdge.id Nothing) -- Pole half-edge has no mating half-edge
        SurfaceCurve3D.Edge _ ->
          case HalfEdge.findMatingHalfEdges halfEdgeSet halfEdge of
            Bag3D.Empty -> Err BoundedBy.BoundaryHasGaps -- No mating half-edge found
            Bag3D.Full (Set3D.Leaf _ matingHalfEdge) -> Ok do
              accumulated
                & HashMap.insert halfEdge.id (Just matingHalfEdge.id)
                & HashMap.insert matingHalfEdge.id (Just halfEdge.id)
            Bag3D.Full Set3D.Node{} -> Err BoundedBy.BoundaryIntersectsItself -- More than one mating half-edge found

----- MESHING -----

toPointMesh :: Tolerance Meters => Resolution Meters -> Body3D space -> Mesh (Point3D space)
toPointMesh resolution body = toMesh resolution Surface3D.pointOn body

toSurfaceMesh ::
  Tolerance Meters =>
  Resolution Meters ->
  Body3D space ->
  Mesh (SurfaceVertex3D space)
toSurfaceMesh resolution body = toMesh resolution Surface3D.vertexOn body

toMesh ::
  Tolerance Meters =>
  Resolution Meters ->
  (Surface3D space -> UvPoint -> vertex) ->
  Body3D space ->
  Mesh vertex
toMesh resolution toVertex body = do
  let surfaceSegmentsEntries = Set3D.toListWithIndex (surfaceSegmentsEntry resolution) body.surfaces
  let surfaceSegmentsMap = HashMap.fromList surfaceSegmentsEntries
  let leadingEdgeVerticesMap = buildLeadingEdgeVerticesMap resolution body surfaceSegmentsMap
  surfaces body
    & Set3D.toListWithIndex (surfaceMesh surfaceSegmentsMap leadingEdgeVerticesMap toVertex)
    & Mesh.concat

surfaceSegmentsEntry ::
  Tolerance Meters =>
  Resolution Meters ->
  Int ->
  Surface3D space ->
  (SurfaceId, Set2D Unitless UvBounds)
surfaceSegmentsEntry resolution surfaceIndex surface = do
  let uvBounds = Region2D.bounds (Surface3D.domain surface)
  let Bounds2D (Interval u1 u2) (Interval v1 v2) = uvBounds
  let surfaceSegmentSet = do
        let p11 = Surface3D.pointAt (UvPoint u1 v1) surface
        let p21 = Surface3D.pointAt (UvPoint u2 v1) surface
        let p12 = Surface3D.pointAt (UvPoint u1 v2) surface
        let p22 = Surface3D.pointAt (UvPoint u2 v2) surface
        buildSurfaceSegmentSet resolution surface uvBounds p11 p21 p12 p22
  (SurfaceId surfaceIndex, surfaceSegmentSet)

buildSurfaceSegmentSet ::
  Tolerance Meters =>
  Resolution Meters ->
  Surface3D space ->
  UvBounds ->
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Set2D Unitless UvBounds
buildSurfaceSegmentSet resolution surface uvRange p11 p21 p12 p22 = do
  let d1 = p21 - p12
  let d2 = p22 - p11
  let size = max (Vector3D.magnitude d1) (Vector3D.magnitude d2)
  let UvBounds uRange vRange = uvRange
  let uMid = Interval.midpoint uRange
  let vMid = Interval.midpoint vRange
  let uvCenter = UvPoint uMid vMid
  let pCenter = Surface3D.pointAt uvCenter surface
  let nCenter = Surface3D.normalDirectionAt uvCenter surface
  let pointError point = Quantity.abs ((point - pCenter) `dot` nCenter)
  let maxCornerError = pointError p11 `max` pointError p12 `max` pointError p21 `max` pointError p22
  let uWidth = Interval.width uRange
  let vWidth = Interval.width vRange
  let uOffset = 0.5 * uWidth * Number.sqrt (3 / 7)
  let vOffset = 0.5 * vWidth * Number.sqrt (3 / 7)
  let uInterior1 = uMid - uOffset
  let uInterior2 = uMid + uOffset
  let vInterior1 = vMid - vOffset
  let vInterior2 = vMid + vOffset
  let interiorError uvPoint = pointError (Surface3D.pointAt uvPoint surface)
  let interiorError11 = interiorError (UvPoint uInterior1 vInterior1)
  let interiorError21 = interiorError (UvPoint uInterior2 vInterior1)
  let interiorError12 = interiorError (UvPoint uInterior1 vInterior2)
  let interiorError22 = interiorError (UvPoint uInterior2 vInterior2)
  let maxError =
        maxCornerError
          `max` interiorError11
          `max` interiorError21
          `max` interiorError12
          `max` interiorError22
  if Resolution.acceptable (#size size) (#error maxError) resolution
    then Set2D.leaf uvRange
    else do
      let Interval u1 u2 = uRange
      let Interval v1 v2 = vRange
      let pMid1 = Surface3D.pointAt (UvPoint uMid v1) surface
      let pMid2 = Surface3D.pointAt (UvPoint uMid v2) surface
      let p1Mid = Surface3D.pointAt (UvPoint u1 vMid) surface
      let p2Mid = Surface3D.pointAt (UvPoint u2 vMid) surface
      let uRange1 = Interval u1 uMid
      let uRange2 = Interval uMid u2
      let vRange1 = Interval v1 vMid
      let vRange2 = Interval vMid v2
      let uvRange11 = UvBounds uRange1 vRange1
      let uvRange21 = UvBounds uRange2 vRange1
      let uvRange12 = UvBounds uRange1 vRange2
      let uvRange22 = UvBounds uRange2 vRange2
      let set11 = buildSurfaceSegmentSet resolution surface uvRange11 p11 pMid1 p1Mid pCenter
      let set21 = buildSurfaceSegmentSet resolution surface uvRange21 pMid1 p21 pCenter p2Mid
      let set12 = buildSurfaceSegmentSet resolution surface uvRange12 p1Mid pCenter p12 pMid2
      let set22 = buildSurfaceSegmentSet resolution surface uvRange22 pCenter p2Mid pMid2 p22
      Set2D.node (NonEmpty.four set11 set21 set12 set22)

unsafeGetCurve :: SurfaceCurve3D space -> Curve3D space
unsafeGetCurve (SurfaceCurve3D.Edge (Surface3D.Edge _ curve)) = curve
unsafeGetCurve (SurfaceCurve3D.Pole _) = error "Mating surface curve should be a valid edge"

buildLeadingEdgeVerticesMap ::
  Tolerance Meters =>
  Resolution Meters ->
  Body3D space ->
  HashMap SurfaceId (Set2D Unitless UvBounds) ->
  HashMap HalfEdge.Id (NonEmpty UvPoint)
buildLeadingEdgeVerticesMap resolution body surfaceSegmentsMap =
  HashMap.empty & do
    forEachWithIndex (surfaces body) \surfaceIndex surface -> do
      let surfaceId = SurfaceId surfaceIndex
      let surfaceSegments = surfaceSegmentsMap @ surfaceId
      let surfaceBoundaries = Surface3D.boundaries surface
      forEachWithIndex surfaceBoundaries \boundaryIndex boundary -> do
        let boundaryId = BoundaryId boundaryIndex
        forEachWithIndex boundary \curveIndex surfaceCurve accumulated -> do
          let curveId = CurveId curveIndex
          let halfEdgeId = HalfEdge.Id{surfaceId, boundaryId, curveId}
          let uvCurve = SurfaceCurve3D.uvCurve surfaceCurve
          case body.seams @ halfEdgeId of
            Nothing -> do
              -- Degenerate half-edge not mated to any adjacent half-edge
              let edgePredicate = degenerateEdgeLinearizationPredicate uvCurve surfaceSegments
              let tValues = Domain1D.leadingSamplingPoints edgePredicate
              let uvPoints = NonEmpty.map (Curve2D.pointOn uvCurve) tValues
              accumulated & HashMap.insert halfEdgeId uvPoints
            Just matingHalfEdgeId ->
              -- The logic below generates mesh vertices for *both* sides of a given half-edge,
              -- so it would be wasteful and redundant to run it once on one half-edge
              -- and then run it again later on the mating half-edge.
              -- So, arbitrarily choose the side with lower half-edge ID to be 'primary'
              -- and only generate vertices when we encounter that side.
              if halfEdgeId < matingHalfEdgeId
                then do
                  let curve = unsafeGetCurve surfaceCurve
                  let uniformParameterization = Curve3D.uniformParameterization curve
                  let matingSurfaceCurve = body @ matingHalfEdgeId
                  let matingSurfaceSegments = surfaceSegmentsMap @ matingHalfEdgeId.surfaceId
                  let matingCurve = unsafeGetCurve matingSurfaceCurve
                  let matingUniformParameterization = Curve3D.uniformParameterization matingCurve
                  let matingUvCurve = SurfaceCurve3D.uvCurve matingSurfaceCurve
                  let edgePredicate =
                        edgeLinearizationPredicate
                          resolution
                          curve
                          uvCurve
                          uniformParameterization
                          matingUvCurve
                          matingUniformParameterization
                          surfaceSegments
                          matingSurfaceSegments
                  let innerRValues = Domain1D.innerSamplingPoints edgePredicate
                  let uvPoint = Curve2D.pointOn uvCurve . uniformParameterization
                  let innerUvPoints = List.map uvPoint innerRValues
                  let uvPoints = Curve2D.startPoint uvCurve :| innerUvPoints
                  let matingInnerRValues = List.reverseMap (1.0 -) innerRValues
                  let matingUvPoint = Curve2D.pointOn matingUvCurve . matingUniformParameterization
                  let matingInnerUvPoints = List.map matingUvPoint matingInnerRValues
                  let matingUvPoints = Curve2D.startPoint matingUvCurve :| matingInnerUvPoints
                  accumulated
                    & HashMap.insert halfEdgeId uvPoints
                    & HashMap.insert matingHalfEdgeId matingUvPoints
                else accumulated

edgeLinearizationPredicate ::
  Resolution Meters ->
  Curve3D space ->
  UvCurve ->
  (Number -> Number) ->
  UvCurve ->
  (Number -> Number) ->
  Set2D Unitless UvBounds ->
  Set2D Unitless UvBounds ->
  Interval Unitless ->
  Bool
edgeLinearizationPredicate
  resolution
  curve
  uvCurve
  uniformParameterization
  matingUvCurve
  matingUniformParameterization
  surfaceSegments
  matingSurfaceSegments
  (Interval rStart rEnd) = do
    let tStart = uniformParameterization rStart
    let tEnd = uniformParameterization rEnd
    let uvStart = Curve2D.pointAt tStart uvCurve
    let uvEnd = Curve2D.pointAt tEnd uvCurve
    let matingTStart = matingUniformParameterization (1.0 - rStart)
    let matingTEnd = matingUniformParameterization (1.0 - rEnd)
    let matingUvStart = Curve2D.pointAt matingTStart matingUvCurve
    let matingUvEnd = Curve2D.pointAt matingTEnd matingUvCurve
    let uvRange = Bounds2D.hull2 uvStart uvEnd
    let matingUvRange = Bounds2D.hull2 matingUvStart matingUvEnd
    let edgeSize = Point2D.distanceFrom uvStart uvEnd
    let matingEdgeSize = Point2D.distanceFrom matingUvStart matingUvEnd
    let startPoint = Curve3D.pointAt tStart curve
    let endPoint = Curve3D.pointAt tEnd curve
    let edgeLength = Point3D.distanceFrom startPoint endPoint
    let edgeLinearDeviation = Curve.linearDeviation curve (Interval tStart tEnd)
    Resolution.acceptable (#size edgeLength) (#error edgeLinearDeviation) resolution
      && validEdge uvRange edgeSize surfaceSegments
      && validEdge matingUvRange matingEdgeSize matingSurfaceSegments

degenerateEdgeLinearizationPredicate ::
  UvCurve ->
  Set2D Unitless UvBounds ->
  Interval Unitless ->
  Bool
degenerateEdgeLinearizationPredicate uvCurve surfaceSegments (Interval tStart tEnd) = do
  let uvStart = Curve2D.pointAt tStart uvCurve
  let uvEnd = Curve2D.pointAt tEnd uvCurve
  let edgeBounds = Bounds2D.hull2 uvStart uvEnd
  let edgeSize = Point2D.distanceFrom uvStart uvEnd
  validEdge edgeBounds edgeSize surfaceSegments

validEdge :: UvBounds -> Number -> Set2D Unitless UvBounds -> Bool
validEdge edgeBounds edgeLength surfaceSegments = unitless do
  case surfaceSegments of
    Set2D.Node nodeBounds children ->
      not (edgeBounds ^ nodeBounds)
        || NonEmpty.all (validEdge edgeBounds edgeLength) children
    Set2D.Leaf leafBounds _ ->
      not (edgeBounds ^ leafBounds)
        || edgeLength <= Number.sqrt 2.0 * Bounds2D.diameter leafBounds

surfaceMesh ::
  Tolerance Meters =>
  HashMap SurfaceId (Set2D Unitless UvBounds) ->
  HashMap HalfEdge.Id (NonEmpty UvPoint) ->
  (Surface3D space -> UvPoint -> vertex) ->
  Int ->
  Surface3D space ->
  Mesh vertex
surfaceMesh surfaceSegmentsMap leadingEdgeVerticesMap toVertex surfaceIndex surface = do
  let surfaceId = SurfaceId surfaceIndex
  let boundaryPolygons =
        Surface3D.boundaries surface
          & Set3D.toNonEmptyWithIndex (toPolygon leadingEdgeVerticesMap surfaceId)
  let boundarySegments = NonEmpty.combine Polygon2D.edges boundaryPolygons
  let boundarySegmentSet = Set2D.build boundarySegments
  let surfaceSegments = surfaceSegmentsMap @ surfaceId
  let steinerPoints =
        if Set2D.size surfaceSegments == 1
          -- If the surface is sufficiently linear to be approximated by a single segment,
          -- then we don't need any interior points at all (can just use the boundary points)
          then []
          else Set2D.toList surfaceSegments & List.filterMap (steinerPoint boundarySegmentSet)
  let boundaryVertexLoops = NonEmpty.map Polygon2D.vertices boundaryPolygons
  let uvPointMesh = CDT.unsafe boundaryVertexLoops steinerPoints
  Mesh.map (toVertex surface) uvPointMesh

toPolygon ::
  HashMap HalfEdge.Id (NonEmpty UvPoint) ->
  SurfaceId ->
  Int ->
  Surface3D.Boundary space ->
  Polygon2D Unitless
toPolygon leadingEdgeVerticesMap surfaceId boundaryIndex boundary =
  Polygon2D $
    NonEmpty.concat $
      Set3D.toNonEmptyWithIndex
        (getLeadingEdgeVertices leadingEdgeVerticesMap surfaceId (BoundaryId boundaryIndex))
        boundary

getLeadingEdgeVertices ::
  HashMap HalfEdge.Id (NonEmpty UvPoint) ->
  SurfaceId ->
  BoundaryId ->
  Int ->
  SurfaceCurve3D space ->
  NonEmpty UvPoint
getLeadingEdgeVertices leadingEdgeVerticesMap surfaceId boundaryId curveIndex _ = do
  let halfEdgeId = HalfEdge.Id{surfaceId, boundaryId, curveId = CurveId curveIndex}
  leadingEdgeVerticesMap @ halfEdgeId

steinerPoint :: Set2D Unitless (Line2D Unitless) -> UvBounds -> Maybe UvPoint
steinerPoint boundarySegmentSet uvRange = do
  let uvPoint = Bounds2D.centerPoint uvRange
  if isValidSteinerPoint uvPoint boundarySegmentSet then Just uvPoint else Nothing

isValidSteinerPoint :: UvPoint -> Set2D Unitless (Line2D Unitless) -> Bool
isValidSteinerPoint uvPoint edgeSet = case edgeSet of
  Set2D.Leaf _ edge -> Line2D.distanceTo uvPoint edge >= 0.5 * Line2D.length edge
  Set2D.Node nodeBounds children ->
    Bounds2D.exclusion uvPoint nodeBounds >= 0.5 * Bounds2D.diameter nodeBounds
      || NonEmpty.all (isValidSteinerPoint uvPoint) children

surfaces :: Body3D space -> Set3D space (Surface3D space)
surfaces = (.surfaces)

orthonormalTransform :: (Surface3D space1 -> Surface3D space2) -> Body3D space1 -> Body3D space2
orthonormalTransform function body =
  Body3D{surfaces = Set3D.map function body.surfaces, seams = body.seams}

-- | Convert a body defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Body3D local -> Body3D global
placeIn frame = orthonormalTransform (Surface3D.placeIn frame)

-- | Convert a body defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Body3D global -> Body3D local
relativeTo frame = orthonormalTransform (Surface3D.relativeTo frame)
