module OpenSolid.Body3D
  ( Body3D
  , block
  , sphere
  , cylinder
  , cylinderAlong
  , extruded
  , translational
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
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Line2D (Line2D)
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate)
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
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceFunction3D.Nondegenerate qualified as SurfaceFunction3D.Nondegenerate
import OpenSolid.SurfaceVertex3D (SurfaceVertex3D (SurfaceVertex3D))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds, pattern UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D
import OpenSolid.World3D qualified as World3D

-- | A solid body in 3D, defined by a set of boundary surfaces.
data Body3D space = Body3D
  { surfaces :: Set3D space (Surface3D space)
  , seams :: HashMap HalfEdge.Id (Maybe Seam)
  }

instance Indexed (Body3D space) SurfaceId (Surface3D space) where
  body !! surfaceId = body.surfaces !! surfaceId

instance Indexed (Body3D space) HalfEdge.Id (SurfaceCurve3D space) where
  body !! HalfEdge.Id{surfaceId, boundaryId, curveId} = body !! surfaceId !! boundaryId !! curveId

-- | Data defining how a half-edge mates to its neighbour.
data Seam = Seam
  { matingHalfEdgeId :: HalfEdge.Id
  , uniformParameterization :: Curve1D Unitless
  , matingUniformParameterization :: Curve1D Unitless
  }

instance FFI (Body3D FFI.Space) where
  representation = FFI.classRepresentation "Body3D"

----- CONSTRUCTION -----

data EmptyBody = EmptyBody deriving (Eq, Show)

{-| Create a rectangular block body.

Fails if the given bounds are empty (the length, width, or height is zero).
-}
block :: Tolerance Meters => Bounds3D space -> Result EmptyBody (Body3D space)
block bounds =
  case Region2D.rectangle (Bounds3D.projectInto World3D.topPlane bounds) of
    Error Region2D.EmptyRegion -> Error EmptyBody
    Ok profile -> do
      let Interval h1 h2 = Bounds3D.upwardCoordinate bounds
      if h1 ~= h2
        then Error EmptyBody
        else case extruded World3D.topPlane profile h1 h2 of
          Ok body -> Ok body
          Error _ ->
            InternalError.throw "Constructing block body from non-empty bounds should not fail"

{-| Create a sphere with the given center point and diameter.

Fails if the given diameter is zero.
-}
sphere ::
  Tolerance Meters =>
  "centerPoint" ::: Point3D space ->
  "diameter" ::: Length ->
  Result EmptyBody (Body3D space)
sphere ("centerPoint" ::: centerPoint) ("diameter" ::: diameter)
  | diameter ~= Length.zero = Error EmptyBody
  | otherwise = do
      let r = 0.5 * diameter
      let arc = Curve2D.arcFrom (Point2D.y r) (Point2D.y -r) -Angle.pi
      let plane = World3D.forwardPlane centerPoint
      case boundedBy [Surface3D.revolved plane arc Axis2D.y Angle.twoPi] of
        Ok body -> Ok body
        Error _ -> InternalError.throw "Constructing sphere from non-zero diameter should not fail"

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
    Error Vector.IsZero -> Error EmptyBody
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
    Error Region2D.EmptyRegion -> Error EmptyBody
    Ok profile ->
      if d1 ~= d2
        then Error EmptyBody
        else case extruded (Axis3D.normalPlane axis) profile d1 d2 of
          Ok body -> Ok body
          Error _ -> InternalError.throw "Constructing non-empty cylinder body should not fail"

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
  translational sketchPlane profile (VectorCurve3D.interpolateFrom v1 v2)

translational ::
  Tolerance Meters =>
  Plane3D space ->
  Region2D Meters ->
  VectorCurve3D Meters space ->
  Result BoundedBy.Error (Body3D space)
translational sketchPlane profile givenDisplacement = do
  -- Fix displacement curve so that extrusion is upwards from plane
  let startDerivative = VectorCurve3D.derivativeValue givenDisplacement 0.0
  let displacement =
        case Quantity.sign (startDerivative `dot` Plane3D.normalDirection sketchPlane) of
          Positive -> givenDisplacement
          Negative -> VectorCurve3D.reverse givenDisplacement
  let startPlane = Plane3D.translateBy (VectorCurve3D.startValue displacement) sketchPlane
  let endPlane = Plane3D.translateBy (VectorCurve3D.endValue displacement) sketchPlane
  let startCap = Surface3D.flip (Surface3D.on startPlane profile)
  let endCap = Surface3D.on endPlane profile
  let sideSurface curve = Surface3D.translational (Curve2D.placeOn sketchPlane curve) displacement
  let sideSurfaces = Set2D.toListOf sideSurface (Region2D.boundaryCurves profile)
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
      Error Curve1D.CrossesZero -> Error BoundedBy.BoundaryIntersectsItself
      Ok curveSigns
        | List.all (== Positive) curveSigns -> Ok Positive
        | List.all (== Negative) curveSigns -> Ok Negative
        | otherwise -> Error BoundedBy.BoundaryIntersectsItself
  let planeRotationAxis = Axis2D.placeOn sketchPlane givenAxis
  let rotatedPlane = Plane3D.rotateAround planeRotationAxis givenSweptAngle sketchPlane
  let (startPlane, endPlane) =
        case profileSign * Quantity.sign givenSweptAngle of
          Positive -> (sketchPlane, rotatedPlane)
          Negative -> (rotatedPlane, sketchPlane)
  let sweptAngle = Quantity.abs givenSweptAngle
  let startCap = Surface3D.flip (Surface3D.on startPlane profile)
  let endCap = Surface3D.on endPlane profile
  let isFullRevolution = Tolerance.using Angle.tolerance (sweptAngle ~= Angle.twoPi)
  let endSurfaces = if isFullRevolution then [] else [startCap, endCap]
  -- A 2D axis such that the profile is to the *left* of the axis
  -- (such that it comes "out of the page" when revolved,
  -- in turn meaning that the side surfaces have the correct normal orientation)
  let axis2D = profileSign * givenAxis
  let sideSurface profileCurve = Surface3D.revolved startPlane profileCurve axis2D sweptAngle
  let sideSurfaces = List.map sideSurface offAxisCurves
  boundedBy (endSurfaces <> sideSurfaces)

{-| Create a body bounded by the given surfaces.
The surfaces do not have to have consistent orientation,
but currently the *first* surface must have the correct orientation
since all others will be flipped if necessary to match it.
-}
boundedBy :: Tolerance Meters => List (Surface3D space) -> Result BoundedBy.Error (Body3D space)
boundedBy [] = Error BoundedBy.EmptyBody
boundedBy (NonEmpty givenSurfaces) = do
  let surfaceSet = Set3D.build Surface3D.bounds givenSurfaces
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
        let curve3D = SurfaceCurve3D.curve surfaceCurve
        let (_, uniformParameterization) = Curve3D.uniformParameterization curve3D
        let halfEdge = HalfEdge{id = halfEdgeId, surfaceCurve, uniformParameterization}
        Set3D.singleton (HalfEdge.bounds halfEdge) halfEdge

registerSeam ::
  Tolerance Meters =>
  Set3D space (HalfEdge space) ->
  HalfEdge space ->
  HashMap HalfEdge.Id (Maybe Seam) ->
  Result BoundedBy.Error (HashMap HalfEdge.Id (Maybe Seam))
registerSeam halfEdgeSet halfEdge accumulated =
  case accumulated & HashMap.lookup halfEdge.id of
    Just _ -> Ok accumulated -- We've already registered this seam from the other side
    Nothing -> do
      let curve3D = HalfEdge.curve halfEdge
      if Curve3D.isPoint curve3D
        then Ok (accumulated & HashMap.insert halfEdge.id Nothing) -- Current half-edge is degenerate
        else case HalfEdge.findMatingHalfEdges halfEdgeSet halfEdge of
          List.One matingHalfEdge -> Ok do
            accumulated
              & HashMap.insert halfEdge.id (Just (seam halfEdge matingHalfEdge))
              & HashMap.insert matingHalfEdge.id (Just (seam matingHalfEdge halfEdge))
          [] -> Error BoundedBy.BoundaryHasGaps
          List.TwoOrMore -> Error BoundedBy.BoundaryIntersectsItself

seam :: HalfEdge space -> HalfEdge space -> Seam
seam halfEdge matingHalfEdge =
  Seam
    { matingHalfEdgeId = matingHalfEdge.id
    , uniformParameterization = halfEdge.uniformParameterization
    , matingUniformParameterization = matingHalfEdge.uniformParameterization
    }

----- MESHING -----

toPointMesh :: Tolerance Meters => Resolution Meters -> Body3D space -> Mesh (Point3D space)
toPointMesh resolution body = body & toMesh resolution SurfaceFunction3D.Nondegenerate.point

toSurfaceMesh ::
  Tolerance Meters =>
  Resolution Meters ->
  Body3D space ->
  Mesh (SurfaceVertex3D space)
toSurfaceMesh resolution body =
  body & toMesh resolution \function uvPoint ->
    SurfaceVertex3D
      (SurfaceFunction3D.Nondegenerate.point function uvPoint)
      (SurfaceFunction3D.Nondegenerate.normalDirectionValue function uvPoint)

toMesh ::
  Tolerance Meters =>
  Resolution Meters ->
  (Nondegenerate (SurfaceFunction3D space) -> UvPoint -> vertex) ->
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
  let surfaceSegmentSet =
        case SurfaceFunction3D.nondegenerate (Surface3D.function surface) of
          Ok function -> do
            let p11 = SurfaceFunction3D.Nondegenerate.point function (UvPoint u1 v1)
            let p21 = SurfaceFunction3D.Nondegenerate.point function (UvPoint u2 v1)
            let p12 = SurfaceFunction3D.Nondegenerate.point function (UvPoint u1 v2)
            let p22 = SurfaceFunction3D.Nondegenerate.point function (UvPoint u2 v2)
            buildSurfaceSegmentSet resolution function uvBounds p11 p21 p12 p22
          Error IsDegenerate -> Set2D.singleton uvBounds uvBounds
  (SurfaceId surfaceIndex, surfaceSegmentSet)

buildSurfaceSegmentSet ::
  Resolution Meters ->
  Nondegenerate (SurfaceFunction3D space) ->
  UvBounds ->
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Point3D space ->
  Set2D Unitless UvBounds
buildSurfaceSegmentSet resolution function uvRange p11 p21 p12 p22 = do
  let d1 = p21 - p12
  let d2 = p22 - p11
  let size = max (Vector3D.magnitude d1) (Vector3D.magnitude d2)
  let UvBounds uRange vRange = uvRange
  let uMid = Interval.midpoint uRange
  let vMid = Interval.midpoint vRange
  let uvCenter = UvPoint uMid vMid
  let pCenter = SurfaceFunction3D.Nondegenerate.point function uvCenter
  let nCenter = SurfaceFunction3D.Nondegenerate.normalDirectionValue function uvCenter
  let error point = Quantity.abs ((point - pCenter) `dot` nCenter)
  let maxCornerError = error p11 `max` error p12 `max` error p21 `max` error p22
  let uWidth = Interval.width uRange
  let vWidth = Interval.width vRange
  let uOffset = 0.5 * uWidth * Number.sqrt (3 / 7)
  let vOffset = 0.5 * vWidth * Number.sqrt (3 / 7)
  let uInterior1 = uMid - uOffset
  let uInterior2 = uMid + uOffset
  let vInterior1 = vMid - vOffset
  let vInterior2 = vMid + vOffset
  let interiorError uvPoint = error (SurfaceFunction3D.Nondegenerate.point function uvPoint)
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
    then Set2D.Leaf uvRange uvRange
    else do
      let Interval u1 u2 = uRange
      let Interval v1 v2 = vRange
      let pMid1 = SurfaceFunction3D.Nondegenerate.point function (UvPoint uMid v1)
      let pMid2 = SurfaceFunction3D.Nondegenerate.point function (UvPoint uMid v2)
      let p1Mid = SurfaceFunction3D.Nondegenerate.point function (UvPoint u1 vMid)
      let p2Mid = SurfaceFunction3D.Nondegenerate.point function (UvPoint u2 vMid)
      let uRange1 = Interval u1 uMid
      let uRange2 = Interval uMid u2
      let vRange1 = Interval v1 vMid
      let vRange2 = Interval vMid v2
      let uvRange11 = UvBounds uRange1 vRange1
      let uvRange21 = UvBounds uRange2 vRange1
      let uvRange12 = UvBounds uRange1 vRange2
      let uvRange22 = UvBounds uRange2 vRange2
      let set11 = buildSurfaceSegmentSet resolution function uvRange11 p11 pMid1 p1Mid pCenter
      let set21 = buildSurfaceSegmentSet resolution function uvRange21 pMid1 p21 pCenter p2Mid
      let set12 = buildSurfaceSegmentSet resolution function uvRange12 p1Mid pCenter p12 pMid2
      let set22 = buildSurfaceSegmentSet resolution function uvRange22 pCenter p2Mid pMid2 p22
      let set1 = Set2D.Node (Bounds2D uRange vRange1) set11 set21
      let set2 = Set2D.Node (Bounds2D uRange vRange2) set12 set22
      Set2D.Node uvRange set1 set2

buildLeadingEdgeVerticesMap ::
  Tolerance Meters =>
  Resolution Meters ->
  Body3D space ->
  HashMap SurfaceId (Set2D Unitless UvBounds) ->
  HashMap HalfEdge.Id (NonEmpty UvPoint)
buildLeadingEdgeVerticesMap resolution body surfaceSegmentsMap =
  HashMap.empty & do
    Set3D.forEachWithIndex (surfaces body) \surfaceIndex surface -> do
      let surfaceId = SurfaceId surfaceIndex
      let surfaceSegments = surfaceSegmentsMap !! surfaceId
      let surfaceBoundaries = Surface3D.boundaries surface
      Set3D.forEachWithIndex surfaceBoundaries \boundaryIndex boundary -> do
        let boundaryId = BoundaryId boundaryIndex
        Set3D.forEachWithIndex boundary \curveIndex surfaceCurve accumulated -> do
          let curveId = CurveId curveIndex
          let halfEdgeId = HalfEdge.Id{surfaceId, boundaryId, curveId}
          let curve3D = SurfaceCurve3D.curve surfaceCurve
          let uvCurve = SurfaceCurve3D.uvCurve surfaceCurve
          case body.seams !! halfEdgeId of
            Nothing -> do
              -- Degenerate half-edge not mated to any adjacent half-edge
              let edgePredicate = degenerateEdgeLinearizationPredicate uvCurve surfaceSegments
              let tValues = Domain1D.leadingSamplingPoints edgePredicate
              let uvPoints = NonEmpty.map (Curve2D.point uvCurve) tValues
              accumulated & HashMap.insert halfEdgeId uvPoints
            Just Seam{matingHalfEdgeId, uniformParameterization, matingUniformParameterization} ->
              -- The logic below generates mesh vertices for *both* sides of a given half-edge,
              -- so it would be wasteful and redundant to run it once on one half-edge
              -- and then run it again later on the mating half-edge.
              -- So, arbitrarily choose the side with lower half-edge ID to be 'primary'
              -- and only generate vertices when we encounter that side.
              if halfEdgeId < matingHalfEdgeId
                then do
                  let matingSurfaceCurve = body !! matingHalfEdgeId
                  let matingSurfaceSegments = surfaceSegmentsMap !! matingHalfEdgeId.surfaceId
                  let matingUvCurve = SurfaceCurve3D.uvCurve matingSurfaceCurve
                  let edgePredicate =
                        edgeLinearizationPredicate
                          resolution
                          curve3D
                          uvCurve
                          uniformParameterization
                          matingUvCurve
                          matingUniformParameterization
                          surfaceSegments
                          matingSurfaceSegments
                  let innerSValues = Domain1D.innerSamplingPoints edgePredicate
                  let innerUvPoints =
                        List.map
                          (Curve2D.point uvCurve . Curve1D.value uniformParameterization)
                          innerSValues
                  let uvPoints = Curve2D.startPoint uvCurve :| innerUvPoints
                  let matingInnerSValues = List.reverseMap (1.0 -) innerSValues
                  let matingInnerUvPoints =
                        List.map
                          (Curve2D.point matingUvCurve . Curve1D.value matingUniformParameterization)
                          matingInnerSValues
                  let matingUvPoints = Curve2D.startPoint matingUvCurve :| matingInnerUvPoints
                  accumulated
                    & HashMap.insert halfEdgeId uvPoints
                    & HashMap.insert matingHalfEdgeId matingUvPoints
                else accumulated

edgeLinearizationPredicate ::
  Resolution Meters ->
  Curve3D space ->
  Curve2D Unitless ->
  Curve1D Unitless ->
  Curve2D Unitless ->
  Curve1D Unitless ->
  Set2D Unitless UvBounds ->
  Set2D Unitless UvBounds ->
  Interval Unitless ->
  Bool
edgeLinearizationPredicate
  resolution
  curve3D
  uvCurve
  uniformParameterization
  matingUvCurve
  matingUniformParameterization
  surfaceSegments
  matingSurfaceSegments
  (Interval rStart rEnd) = do
    let tStart = Curve1D.value uniformParameterization rStart
    let tEnd = Curve1D.value uniformParameterization rEnd
    let uvStart = Curve2D.point uvCurve tStart
    let uvEnd = Curve2D.point uvCurve tEnd
    let matingRStart = Curve1D.value matingUniformParameterization (1.0 - rStart)
    let matingREnd = Curve1D.value matingUniformParameterization (1.0 - rEnd)
    let matingUvStart = Curve2D.point matingUvCurve matingRStart
    let matingUvEnd = Curve2D.point matingUvCurve matingREnd
    let uvRange = Bounds2D.hull2 uvStart uvEnd
    let matingUvRange = Bounds2D.hull2 matingUvStart matingUvEnd
    let edgeSize = Point2D.distanceFrom uvStart uvEnd
    let matingEdgeSize = Point2D.distanceFrom matingUvStart matingUvEnd
    let startPoint = Curve3D.point curve3D tStart
    let endPoint = Curve3D.point curve3D tEnd
    let edgeLength = Point3D.distanceFrom startPoint endPoint
    let edgeLinearDeviation = Curve.linearDeviation curve3D (Interval tStart tEnd)
    Resolution.acceptable (#size edgeLength) (#error edgeLinearDeviation) resolution
      && validEdge uvRange edgeSize surfaceSegments
      && validEdge matingUvRange matingEdgeSize matingSurfaceSegments

degenerateEdgeLinearizationPredicate ::
  Curve2D Unitless ->
  Set2D Unitless UvBounds ->
  Interval Unitless ->
  Bool
degenerateEdgeLinearizationPredicate uvCurve surfaceSegments (Interval tStart tEnd) = do
  let uvStart = Curve2D.point uvCurve tStart
  let uvEnd = Curve2D.point uvCurve tEnd
  let edgeBounds = Bounds2D.hull2 uvStart uvEnd
  let edgeSize = Point2D.distanceFrom uvStart uvEnd
  validEdge edgeBounds edgeSize surfaceSegments

validEdge :: UvBounds -> Number -> Set2D Unitless UvBounds -> Bool
validEdge edgeBounds edgeLength surfaceSegments = Tolerance.using Tolerance.unitless do
  case surfaceSegments of
    Set2D.Node nodeBounds left right ->
      not (intersects edgeBounds nodeBounds)
        || (validEdge edgeBounds edgeLength left && validEdge edgeBounds edgeLength right)
    Set2D.Leaf leafBounds _ ->
      not (intersects edgeBounds leafBounds)
        || edgeLength <= Number.sqrt 2.0 * Bounds2D.diameter leafBounds

surfaceMesh ::
  Tolerance Meters =>
  HashMap SurfaceId (Set2D Unitless UvBounds) ->
  HashMap HalfEdge.Id (NonEmpty UvPoint) ->
  (Nondegenerate (SurfaceFunction3D space) -> UvPoint -> vertex) ->
  Int ->
  Surface3D space ->
  Mesh vertex
surfaceMesh surfaceSegmentsMap leadingEdgeVerticesMap toVertex surfaceIndex surface =
  case SurfaceFunction3D.nondegenerate (Surface3D.function surface) of
    Error IsDegenerate -> Mesh.empty
    Ok nondegenerateSurfaceFunction -> do
      let surfaceId = SurfaceId surfaceIndex
      let boundaryPolygons =
            Surface3D.boundaries surface
              & Set3D.toNonEmptyWithIndex (toPolygon leadingEdgeVerticesMap surfaceId)
      let boundarySegments = NonEmpty.combine Polygon2D.edges boundaryPolygons
      let boundarySegmentSet = Set2D.build Line2D.bounds boundarySegments
      let surfaceSegments = surfaceSegmentsMap !! surfaceId
      let steinerPoints =
            if Set2D.size surfaceSegments == 1
              -- If the surface is sufficiently linear to be approximated by a single segment,
              -- then we don't need any interior points at all (can just use the boundary points)
              then []
              else Set2D.toList surfaceSegments & List.filterMap (steinerPoint boundarySegmentSet)
      let boundaryVertexLoops = NonEmpty.map Polygon2D.vertices boundaryPolygons
      let uvPointMesh = CDT.unsafe boundaryVertexLoops steinerPoints
      Mesh.map (toVertex nondegenerateSurfaceFunction) uvPointMesh

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
  leadingEdgeVerticesMap !! halfEdgeId

steinerPoint :: Set2D Unitless (Line2D Unitless) -> UvBounds -> Maybe UvPoint
steinerPoint boundarySegmentSet uvRange = do
  let uvPoint = Bounds2D.centerPoint uvRange
  if isValidSteinerPoint boundarySegmentSet uvPoint then Just uvPoint else Nothing

isValidSteinerPoint :: Set2D Unitless (Line2D Unitless) -> UvPoint -> Bool
isValidSteinerPoint edgeSet uvPoint = case edgeSet of
  Set2D.Leaf _ edge -> Line2D.distanceTo uvPoint edge >= 0.5 * Line2D.length edge
  Set2D.Node nodeBounds left right ->
    Bounds2D.exclusion uvPoint nodeBounds >= 0.5 * Bounds2D.diameter nodeBounds
      || (isValidSteinerPoint left uvPoint && isValidSteinerPoint right uvPoint)

surfaces :: Body3D space -> Set3D space (Surface3D space)
surfaces = (.surfaces)

orthonormalTransform :: (Surface3D space1 -> Surface3D space2) -> Body3D space1 -> Body3D space2
orthonormalTransform function body =
  Body3D{surfaces = Set3D.map function Surface3D.bounds body.surfaces, seams = body.seams}

-- | Convert a body defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Body3D local -> Body3D global
placeIn frame = orthonormalTransform (Surface3D.placeIn frame)

-- | Convert a body defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Body3D global -> Body3D local
relativeTo frame = orthonormalTransform (Surface3D.relativeTo frame)
