module OpenSolid.Surface3D
  ( Surface3D (function, domain, outerLoop, innerLoops)
  , parametric
  , on
  , extruded
  , translational
  , ruled
  , revolved
  , bounds
  , boundaryCurves
  , boundarySurfaceCurves
  , flip
  , placeIn
  , relativeTo
  , toMesh
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.CDT qualified as CDT
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D (Curve3D)
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Length (Length)
import OpenSolid.Line2D (Line2D)
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Polygon2D (Polygon2D (Polygon2D))
import OpenSolid.Polygon2D qualified as Polygon2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set2D qualified as Set2D
import OpenSolid.Surface3D.Revolved qualified as Revolved
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfaceLinearization qualified as SurfaceLinearization
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data Surface3D space = Surface3D
  { function :: SurfaceFunction3D space
  , domain :: Region2D Unitless UvSpace
  , outerLoop :: ~(NonEmpty (SurfaceCurve3D space))
  , innerLoops :: ~(List (NonEmpty (SurfaceCurve3D space)))
  }

parametric ::
  SurfaceFunction3D space ->
  Region2D Unitless UvSpace ->
  Surface3D space
parametric givenFunction givenDomain = do
  let boundaryLoop domainLoop = NonEmpty.map (SurfaceCurve3D.on givenFunction) domainLoop
  Surface3D
    { function = givenFunction
    , domain = givenDomain
    , outerLoop = boundaryLoop (Region2D.outerLoop givenDomain)
    , innerLoops = List.map boundaryLoop (Region2D.innerLoops givenDomain)
    }

on :: Plane3D global local -> Region2D Meters local -> Surface3D global
on plane region = do
  let regionBounds = Region2D.bounds region
  let (width, height) = Bounds2D.dimensions regionBounds
  let centerPoint = Bounds2D.centerPoint regionBounds
  let centerFrame = Frame2D.atPoint centerPoint
  let regionSize = max width height
  let centeredRegion = Region2D.relativeTo centerFrame region
  let normalizedRegion = Region2D.convert (1 /? regionSize) centeredRegion
  let p0 = Point2D.placeOn plane centerPoint
  let vx = regionSize .*. Plane3D.xDirection plane
  let vy = regionSize .*. Plane3D.yDirection plane
  let planeFunction = p0 .+. SurfaceFunction.u .*. vx .+. SurfaceFunction.v .*. vy
  parametric planeFunction normalizedRegion

extruded :: Curve3D space -> Vector3D Meters space -> Surface3D space
extruded curve displacement =
  parametric
    (curve `compose` SurfaceFunction.u .+. SurfaceFunction.v .*. displacement)
    Region2D.unitSquare

translational :: Curve3D space -> VectorCurve3D Meters space -> Surface3D space
translational uCurve vCurve =
  parametric
    (uCurve `compose` SurfaceFunction.u .+. vCurve `compose` SurfaceFunction.v)
    Region2D.unitSquare

ruled :: Curve3D space -> Curve3D space -> Surface3D space
ruled bottom top = do
  let f1 = bottom `compose` SurfaceFunction.u
  let f2 = top `compose` SurfaceFunction.u
  parametric (f1 .+. SurfaceFunction.v .*. (f2 .-. f1)) Region2D.unitSquare

revolved ::
  Tolerance Meters =>
  Plane3D global local ->
  Curve2D Meters local ->
  Axis2D Meters local ->
  Angle ->
  Result Revolved.Error (Surface3D global)
revolved sketchPlane curve axis angle = do
  let frame2D = Frame2D.fromYAxis axis
  let localCurve = Curve2D.relativeTo frame2D curve
  let xCoordinate = localCurve.xCoordinate
  if xCoordinate ~= Curve.zero
    then Error Revolved.ProfileIsOnAxis
    else case Curve.sign xCoordinate of
      Error Curve.CrossesZero -> Error Revolved.ProfileCrossesAxis
      Ok profileSign -> do
        let frame3D = Frame3D.fromBackPlane (Frame2D.placeOn sketchPlane frame2D)
        let (revolutionParameter, curveParameter) = case profileSign of
              Positive -> (SurfaceFunction.u, SurfaceFunction.v)
              Negative -> (SurfaceFunction.v, SurfaceFunction.u)
        let theta = angle .*. revolutionParameter
        let radius = xCoordinate `compose` curveParameter
        let height = localCurve.yCoordinate `compose` curveParameter
        let function =
              frame3D.originPoint
                .+. radius .*. SurfaceFunction.cos theta .*. frame3D.rightwardDirection
                .+. radius .*. SurfaceFunction.sin theta .*. frame3D.forwardDirection
                .+. height .*. frame3D.upwardDirection
        Ok (parametric function Region2D.unitSquare)

bounds :: Surface3D space -> Bounds3D space
bounds surface = SurfaceFunction3D.evaluateBounds surface.function (Region2D.bounds surface.domain)

boundaryCurves :: Surface3D space -> NonEmpty (Curve3D space)
boundaryCurves surface = NonEmpty.map SurfaceCurve3D.curve (boundarySurfaceCurves surface)

boundarySurfaceCurves :: Surface3D space -> NonEmpty (SurfaceCurve3D space)
boundarySurfaceCurves surface = NonEmpty.concat (surface.outerLoop :| surface.innerLoops)

flip :: Surface3D space -> Surface3D space
flip surface =
  parametric
    (surface.function `compose` SurfaceFunction2D.xy (negative SurfaceFunction.u) SurfaceFunction.v)
    (Region2D.mirrorAcross Axis2D.y surface.domain)

-- | Convert a surface defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3D global local -> Surface3D local -> Surface3D global
placeIn frame surface =
  parametric (SurfaceFunction3D.placeIn frame surface.function) surface.domain

-- | Convert a surface defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3D global local -> Surface3D global -> Surface3D local
relativeTo frame surface =
  parametric (SurfaceFunction3D.relativeTo frame surface.function) surface.domain

toMesh :: Length -> Surface3D space -> Mesh (Point3D space)
toMesh accuracy surface = do
  let f = surface.function
  let fuu = f.du.du
  let fuv = f.du.dv
  let fvv = f.dv.dv
  let boundaryLoops = Region2D.boundaryLoops surface.domain
  let boundaryPolygons = NonEmpty.map (toPolygon accuracy f fuu fuv fvv) boundaryLoops
  let boundaryEdges = NonEmpty.combine Polygon2D.edges boundaryPolygons
  let edgeSet = Set2D.fromNonEmpty boundaryEdges
  let domainBounds = Region2D.bounds surface.domain
  let steinerPoints = generateSteinerPoints accuracy domainBounds edgeSet fuu fuv fvv []
  let boundaryVertexLoops = NonEmpty.map Polygon2D.vertices boundaryPolygons
  let uvMesh = CDT.unsafe boundaryVertexLoops steinerPoints
  Mesh.map (SurfaceFunction3D.evaluate f) uvMesh

toPolygon ::
  Length ->
  SurfaceFunction3D space ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  NonEmpty (Curve2D Unitless UvSpace) ->
  Polygon2D Unitless UvSpace
toPolygon accuracy f fuu fuv fvv loop =
  Polygon2D (NonEmpty.combine (boundaryPoints accuracy f fuu fuv fvv) loop)

boundaryPoints ::
  Length ->
  SurfaceFunction3D space ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  Curve2D Unitless UvSpace ->
  NonEmpty UvPoint
boundaryPoints accuracy surfaceFunction fuu fuv fvv uvCurve = do
  let curve3D = surfaceFunction `compose` uvCurve
  let secondDerivative3D = curve3D.derivative.derivative
  let predicate = linearizationPredicate accuracy fuu fuv fvv uvCurve secondDerivative3D
  let parameterValues = Domain1D.leadingSamplingPoints predicate
  NonEmpty.map (Curve2D.evaluate uvCurve) parameterValues

surfaceError ::
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  UvBounds ->
  Length
surfaceError fuu fuv fvv uvBounds = do
  let fuuBounds = VectorSurfaceFunction3D.evaluateBounds fuu uvBounds
  let fuvBounds = VectorSurfaceFunction3D.evaluateBounds fuv uvBounds
  let fvvBounds = VectorSurfaceFunction3D.evaluateBounds fvv uvBounds
  SurfaceLinearization.error fuuBounds fuvBounds fvvBounds uvBounds

linearizationPredicate ::
  Length ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  Curve2D Unitless UvSpace ->
  VectorCurve3D Meters space ->
  Interval Unitless ->
  Bool
linearizationPredicate accuracy fuu fuv fvv curve2D secondDerivative3D subdomain = do
  let curveSecondDerivativeBounds = VectorCurve3D.evaluateBounds secondDerivative3D subdomain
  let curveSecondDerivativeMagnitude = VectorBounds3D.magnitude curveSecondDerivativeBounds
  let uvBounds = Curve2D.evaluateBounds curve2D subdomain
  Linearization.error curveSecondDerivativeMagnitude subdomain <= accuracy
    && surfaceError fuu fuv fvv uvBounds <= accuracy

generateSteinerPoints ::
  Length ->
  UvBounds ->
  Set2D (Line2D Unitless UvSpace) Unitless UvSpace ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  VectorSurfaceFunction3D Meters space ->
  List UvPoint ->
  List UvPoint
generateSteinerPoints accuracy uvBounds edgeSet fuu fuv fvv accumulated = do
  let Bounds2D uBounds vBounds = uvBounds
  let recurse = do
        let (u1, u2) = Interval.bisect uBounds
        let (v1, v2) = Interval.bisect vBounds
        let bounds11 = Bounds2D u1 v1
        let bounds12 = Bounds2D u1 v2
        let bounds21 = Bounds2D u2 v1
        let bounds22 = Bounds2D u2 v2
        accumulated
          & generateSteinerPoints accuracy bounds11 edgeSet fuu fuv fvv
          & generateSteinerPoints accuracy bounds12 edgeSet fuu fuv fvv
          & generateSteinerPoints accuracy bounds21 edgeSet fuu fuv fvv
          & generateSteinerPoints accuracy bounds22 edgeSet fuu fuv fvv
  case includeSubdomain uvBounds edgeSet of
    Unresolved -> recurse
    Resolved False -> accumulated
    Resolved True ->
      if surfaceError fuu fuv fvv uvBounds <= accuracy
        then UvPoint (Interval.midpoint uBounds) (Interval.midpoint vBounds) : accumulated
        else recurse

includeSubdomain :: UvBounds -> Set2D (Line2D Unitless UvSpace) Unitless UvSpace -> Fuzzy Bool
includeSubdomain subdomain edgeSet = Tolerance.using Quantity.zero $
  case edgeSet of
    Set2D.Node nodeBounds leftChild rightChild
      | not (subdomain `intersects` nodeBounds) -> Resolved True
      | smallerThan nodeBounds subdomain -> do
          includeLeft <- includeSubdomain subdomain leftChild
          includeRight <- includeSubdomain subdomain rightChild
          Resolved (includeLeft && includeRight)
      | otherwise -> Unresolved
    Set2D.Leaf edgeBounds _
      | not (subdomain `intersects` edgeBounds) -> Resolved True
      | smallerThan edgeBounds subdomain -> Resolved False
      | otherwise -> Unresolved

smallerThan :: Bounds2D units space -> Bounds2D units space -> Bool
smallerThan bounds1 bounds2 = Bounds2D.diameter bounds2 <= Bounds2D.diameter bounds1
