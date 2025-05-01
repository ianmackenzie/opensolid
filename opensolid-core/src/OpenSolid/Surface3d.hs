module OpenSolid.Surface3d
  ( Surface3d
  , parametric
  , planar
  , extruded
  , translational
  , ruled
  , function
  , domain
  , outerLoop
  , innerLoops
  , boundaryCurves
  , flip
  , toMesh
  )
where

import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CDT qualified as CDT
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.LineSegment2d (LineSegment2d)
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Set2d (Set2d)
import OpenSolid.Set2d qualified as Set2d
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceLinearization qualified as SurfaceLinearization
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data Surface3d (coordinateSystem :: CoordinateSystem) where
  Surface3d ::
    { function :: SurfaceFunction3d (space @ units)
    , domain :: Region2d UvCoordinates
    , outerLoop :: ~(NonEmpty (Curve3d (space @ units)))
    , innerLoops :: ~(List (NonEmpty (Curve3d (space @ units))))
    } ->
    Surface3d (space @ units)

parametric ::
  SurfaceFunction3d (space @ units) ->
  Region2d UvCoordinates ->
  Surface3d (space @ units)
parametric givenFunction givenDomain = do
  let boundaryLoop domainLoop = NonEmpty.map (givenFunction .) domainLoop
  Surface3d
    { function = givenFunction
    , domain = givenDomain
    , outerLoop = boundaryLoop (Region2d.outerLoop givenDomain)
    , innerLoops = List.map boundaryLoop (Region2d.innerLoops givenDomain)
    }

planar ::
  Plane3d (space @ units) (Defines local) ->
  Region2d (local @ units) ->
  Surface3d (space @ units)
planar plane region = do
  let regionBounds = Region2d.bounds region
  let (width, height) = Bounds2d.dimensions regionBounds
  let centerPoint = Bounds2d.centerPoint regionBounds
  let centerFrame = Frame2d.atPoint centerPoint
  let regionSize = Qty.max width height
  let centeredRegion = Region2d.relativeTo centerFrame region
  let normalizedRegion = Region2d.convert (1.0 ./. regionSize) centeredRegion
  let p0 = Point2d.placeOn plane centerPoint
  let vx = regionSize * Plane3d.xDirection plane
  let vy = regionSize * Plane3d.yDirection plane
  let planeFunction = p0 + SurfaceFunction.u * vx + SurfaceFunction.v * vy
  parametric planeFunction normalizedRegion

extruded :: Curve3d (space @ units) -> Vector3d (space @ units) -> Surface3d (space @ units)
extruded curve displacement =
  parametric (curve . SurfaceFunction.u + SurfaceFunction.v * displacement) Region2d.unitSquare

translational ::
  Curve3d (space @ units) ->
  VectorCurve3d (space @ units) ->
  Surface3d (space @ units)
translational uCurve vCurve =
  parametric (uCurve . SurfaceFunction.u + vCurve . SurfaceFunction.v) Region2d.unitSquare

ruled :: Curve3d (space @ units) -> Curve3d (space @ units) -> Surface3d (space @ units)
ruled bottom top = do
  let f1 = bottom . SurfaceFunction.u
  let f2 = top . SurfaceFunction.u
  parametric (f1 + SurfaceFunction.v * (f2 - f1)) Region2d.unitSquare

boundaryCurves :: Surface3d (space @ units) -> NonEmpty (Curve3d (space @ units))
boundaryCurves surface = NonEmpty.concat (outerLoop surface :| innerLoops surface)

flip :: Surface3d (space @ units) -> Surface3d (space @ units)
flip surface =
  parametric
    (function surface . SurfaceFunction2d.xy (1.0 - SurfaceFunction.u) SurfaceFunction.v)
    (Region2d.mirrorAcross Axis2d.y (domain surface))

toMesh :: Qty units -> Surface3d (space @ units) -> Mesh (Point3d (space @ units))
toMesh accuracy surface = do
  let surfaceFunction = function surface
  let surfaceDomain = domain surface
  let fu = SurfaceFunction3d.derivative U surfaceFunction
  let fv = SurfaceFunction3d.derivative V surfaceFunction
  let fuu = VectorSurfaceFunction3d.derivative U fu
  let fuv = VectorSurfaceFunction3d.derivative V fu
  let fvv = VectorSurfaceFunction3d.derivative V fv
  let boundaryLoops = Region2d.outerLoop surfaceDomain :| Region2d.innerLoops surfaceDomain
  let boundaryPolygons = NonEmpty.map (toPolygon accuracy surfaceFunction fuu fuv fvv) boundaryLoops
  let boundaryEdges = NonEmpty.collect Polygon2d.edges boundaryPolygons
  let edgeSet = Set2d.fromNonEmpty boundaryEdges
  let steinerPoints = generateSteinerPoints accuracy (Region2d.bounds surfaceDomain) edgeSet fuu fuv fvv []
  let boundaryVertexLoops = NonEmpty.map Polygon2d.vertices boundaryPolygons
  let uvMesh = CDT.unsafe boundaryVertexLoops steinerPoints
  Mesh.map (SurfaceFunction3d.evaluate surfaceFunction) uvMesh

toPolygon ::
  Qty units ->
  SurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  NonEmpty (Curve2d UvCoordinates) ->
  Polygon2d UvPoint
toPolygon accuracy f fuu fuv fvv loop =
  Polygon2d (NonEmpty.collect (boundaryPoints accuracy f fuu fuv fvv) loop)

boundaryPoints ::
  Qty units ->
  SurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  Curve2d UvCoordinates ->
  NonEmpty UvPoint
boundaryPoints accuracy surfaceFunction fuu fuv fvv uvCurve = do
  let curve3d = surfaceFunction . uvCurve
  let secondDerivative3d = VectorCurve3d.derivative (Curve3d.derivative curve3d)
  let predicate = linearizationPredicate accuracy fuu fuv fvv uvCurve secondDerivative3d
  let parameterValues = Domain1d.leadingSamplingPoints predicate
  NonEmpty.map (Curve2d.evaluate uvCurve) parameterValues

surfaceError ::
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  UvBounds ->
  Qty units
surfaceError fuu fuv fvv uvBounds = do
  let fuuBounds = VectorSurfaceFunction3d.evaluateBounds fuu uvBounds
  let fuvBounds = VectorSurfaceFunction3d.evaluateBounds fuv uvBounds
  let fvvBounds = VectorSurfaceFunction3d.evaluateBounds fvv uvBounds
  SurfaceLinearization.error fuuBounds fuvBounds fvvBounds uvBounds

linearizationPredicate ::
  Qty units ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  Curve2d UvCoordinates ->
  VectorCurve3d (space @ units) ->
  Bounds Unitless ->
  Bool
linearizationPredicate accuracy fuu fuv fvv curve2d secondDerivative3d subdomain = do
  let curveSecondDerivativeBounds = VectorCurve3d.evaluateBounds secondDerivative3d subdomain
  let curveSecondDerivativeMagnitude = VectorBounds3d.magnitude curveSecondDerivativeBounds
  let uvBounds = Curve2d.evaluateBounds curve2d subdomain
  Linearization.error curveSecondDerivativeMagnitude subdomain <= accuracy
    && surfaceError fuu fuv fvv uvBounds <= accuracy

generateSteinerPoints ::
  Qty units ->
  UvBounds ->
  Set2d (LineSegment2d UvPoint) UvCoordinates ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units) ->
  List UvPoint ->
  List UvPoint
generateSteinerPoints accuracy uvBounds edgeSet fuu fuv fvv accumulated = do
  let Bounds2d uBounds vBounds = uvBounds
  let recurse = do
        let (u1, u2) = Bounds.bisect uBounds
        let (v1, v2) = Bounds.bisect vBounds
        let bounds11 = Bounds2d u1 v1
        let bounds12 = Bounds2d u1 v2
        let bounds21 = Bounds2d u2 v1
        let bounds22 = Bounds2d u2 v2
        accumulated
          |> generateSteinerPoints accuracy bounds11 edgeSet fuu fuv fvv
          |> generateSteinerPoints accuracy bounds12 edgeSet fuu fuv fvv
          |> generateSteinerPoints accuracy bounds21 edgeSet fuu fuv fvv
          |> generateSteinerPoints accuracy bounds22 edgeSet fuu fuv fvv
  let return = Point2d (Bounds.midpoint uBounds) (Bounds.midpoint vBounds) : accumulated
  case includeSubdomain uvBounds edgeSet of
    Resolved False -> accumulated
    Resolved True -> if surfaceError fuu fuv fvv uvBounds <= accuracy then return else recurse
    Unresolved -> recurse

includeSubdomain :: UvBounds -> Set2d (LineSegment2d UvPoint) UvCoordinates -> Fuzzy Bool
includeSubdomain subdomain edgeSet = Tolerance.exactly $
  case edgeSet of
    Set2d.Node nodeBounds leftChild rightChild
      | not (subdomain ^ nodeBounds) -> Resolved True
      | smallerThan nodeBounds subdomain -> Fuzzy.do
          includeLeft <- includeSubdomain subdomain leftChild
          includeRight <- includeSubdomain subdomain rightChild
          Resolved (includeLeft && includeRight)
      | otherwise -> Unresolved
    Set2d.Leaf edgeBounds _
      | not (subdomain ^ edgeBounds) -> Resolved True
      | smallerThan edgeBounds subdomain -> Resolved False
      | otherwise -> Unresolved

smallerThan :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
smallerThan bounds1 bounds2 = Bounds2d.diameter bounds2 <= Bounds2d.diameter bounds1
