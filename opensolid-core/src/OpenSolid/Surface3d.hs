module OpenSolid.Surface3d
  ( Surface3d (function, domain, outerLoop, innerLoops)
  , parametric
  , on
  , extruded
  , translational
  , ruled
  , revolved
  , boundaryCurves
  , flip
  , placeIn
  , relativeTo
  , toMesh
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CDT qualified as CDT
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Length (Length)
import OpenSolid.LineSegment2d (LineSegment2d)
import OpenSolid.Linearization qualified as Linearization
import OpenSolid.List qualified as List
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3d (Point3d)
import OpenSolid.Polygon2d (Polygon2d (Polygon2d))
import OpenSolid.Polygon2d qualified as Polygon2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2d (Region2d)
import OpenSolid.Region2d qualified as Region2d
import OpenSolid.Set2d (Set2d)
import OpenSolid.Set2d qualified as Set2d
import OpenSolid.Surface3d.Revolved qualified as Revolved
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import OpenSolid.SurfaceFunction3d (SurfaceFunction3d)
import OpenSolid.SurfaceFunction3d qualified as SurfaceFunction3d
import OpenSolid.SurfaceLinearization qualified as SurfaceLinearization
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data Surface3d space = Surface3d
  { function :: SurfaceFunction3d space
  , domain :: Region2d Unitless UvSpace
  , outerLoop :: ~(NonEmpty (Curve3d space))
  , innerLoops :: ~(List (NonEmpty (Curve3d space)))
  }

parametric ::
  SurfaceFunction3d space ->
  Region2d Unitless UvSpace ->
  Surface3d space
parametric givenFunction givenDomain = do
  let boundaryLoop domainLoop = NonEmpty.map (givenFunction `compose`) domainLoop
  Surface3d
    { function = givenFunction
    , domain = givenDomain
    , outerLoop = boundaryLoop (Region2d.outerLoop givenDomain)
    , innerLoops = List.map boundaryLoop (Region2d.innerLoops givenDomain)
    }

on :: Plane3d global local -> Region2d Meters local -> Surface3d global
on plane region = do
  let regionBounds = Region2d.bounds region
  let (width, height) = Bounds2d.dimensions regionBounds
  let centerPoint = Bounds2d.centerPoint regionBounds
  let centerFrame = Frame2d.atPoint centerPoint
  let regionSize = max width height
  let centeredRegion = Region2d.relativeTo centerFrame region
  let normalizedRegion = Region2d.convert (1 /? regionSize) centeredRegion
  let p0 = Point2D.placeOn plane centerPoint
  let vx = regionSize .*. Plane3d.xDirection plane
  let vy = regionSize .*. Plane3d.yDirection plane
  let planeFunction = p0 .+. SurfaceFunction.u .*. vx .+. SurfaceFunction.v .*. vy
  parametric planeFunction normalizedRegion

extruded :: Curve3d space -> Vector3d Meters space -> Surface3d space
extruded curve displacement =
  parametric
    (curve `compose` SurfaceFunction.u .+. SurfaceFunction.v .*. displacement)
    Region2d.unitSquare

translational :: Curve3d space -> VectorCurve3d Meters space -> Surface3d space
translational uCurve vCurve =
  parametric
    (uCurve `compose` SurfaceFunction.u .+. vCurve `compose` SurfaceFunction.v)
    Region2d.unitSquare

ruled :: Curve3d space -> Curve3d space -> Surface3d space
ruled bottom top = do
  let f1 = bottom `compose` SurfaceFunction.u
  let f2 = top `compose` SurfaceFunction.u
  parametric (f1 .+. SurfaceFunction.v .*. (f2 .-. f1)) Region2d.unitSquare

revolved ::
  Tolerance Meters =>
  Plane3d global local ->
  Curve2d Meters local ->
  Axis2d Meters local ->
  Angle ->
  Result Revolved.Error (Surface3d global)
revolved sketchPlane curve axis angle = do
  let frame2d = Frame2d.fromYAxis axis
  let localCurve = Curve2d.relativeTo frame2d curve
  let xCoordinate = localCurve.xCoordinate
  if xCoordinate ~= Curve.zero
    then Error Revolved.ProfileIsOnAxis
    else case Curve.sign xCoordinate of
      Error Curve.CrossesZero -> Error Revolved.ProfileCrossesAxis
      Ok profileSign -> do
        let frame3d = Frame3d.fromBackPlane (Frame2d.placeOn sketchPlane frame2d)
        let (revolutionParameter, curveParameter) = case profileSign of
              Positive -> (SurfaceFunction.u, SurfaceFunction.v)
              Negative -> (SurfaceFunction.v, SurfaceFunction.u)
        let theta = angle .*. revolutionParameter
        let radius = xCoordinate `compose` curveParameter
        let height = localCurve.yCoordinate `compose` curveParameter
        let function =
              frame3d.originPoint
                .+. radius .*. SurfaceFunction.cos theta .*. frame3d.rightwardDirection
                .+. radius .*. SurfaceFunction.sin theta .*. frame3d.forwardDirection
                .+. height .*. frame3d.upwardDirection
        Ok (parametric function Region2d.unitSquare)

boundaryCurves :: Surface3d space -> NonEmpty (Curve3d space)
boundaryCurves surface = NonEmpty.concat (surface.outerLoop :| surface.innerLoops)

flip :: Surface3d space -> Surface3d space
flip surface =
  parametric
    (surface.function `compose` SurfaceFunction2d.xy (negative SurfaceFunction.u) SurfaceFunction.v)
    (Region2d.mirrorAcross Axis2d.y surface.domain)

-- | Convert a surface defined in local coordinates to one defined in global coordinates.
placeIn :: Frame3d global local -> Surface3d local -> Surface3d global
placeIn frame surface =
  parametric (SurfaceFunction3d.placeIn frame surface.function) surface.domain

-- | Convert a surface defined in global coordinates to one defined in local coordinates.
relativeTo :: Frame3d global local -> Surface3d global -> Surface3d local
relativeTo frame surface =
  parametric (SurfaceFunction3d.relativeTo frame surface.function) surface.domain

toMesh :: Length -> Surface3d space -> Mesh (Point3d space)
toMesh accuracy surface = do
  let f = surface.function
  let fuu = f.du.du
  let fuv = f.du.dv
  let fvv = f.dv.dv
  let boundaryLoops = Region2d.boundaryLoops surface.domain
  let boundaryPolygons =
        NonEmpty.map (toPolygon accuracy surface.function fuu fuv fvv) boundaryLoops
  let boundaryEdges = NonEmpty.combine Polygon2d.edges boundaryPolygons
  let edgeSet = Set2d.fromNonEmpty boundaryEdges
  let domainBounds = Region2d.bounds surface.domain
  let steinerPoints = generateSteinerPoints accuracy domainBounds edgeSet fuu fuv fvv []
  let boundaryVertexLoops = NonEmpty.map Polygon2d.vertices boundaryPolygons
  let uvMesh = CDT.unsafe boundaryVertexLoops steinerPoints
  Mesh.map (SurfaceFunction3d.evaluate surface.function) uvMesh

toPolygon ::
  Length ->
  SurfaceFunction3d space ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  NonEmpty (Curve2d Unitless UvSpace) ->
  Polygon2d Unitless UvSpace
toPolygon accuracy f fuu fuv fvv loop =
  Polygon2d (NonEmpty.combine (boundaryPoints accuracy f fuu fuv fvv) loop)

boundaryPoints ::
  Length ->
  SurfaceFunction3d space ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  Curve2d Unitless UvSpace ->
  NonEmpty UvPoint
boundaryPoints accuracy surfaceFunction fuu fuv fvv uvCurve = do
  let curve3d = surfaceFunction `compose` uvCurve
  let secondDerivative3d = curve3d.derivative.derivative
  let predicate = linearizationPredicate accuracy fuu fuv fvv uvCurve secondDerivative3d
  let parameterValues = Domain1d.leadingSamplingPoints predicate
  NonEmpty.map (Curve2d.evaluate uvCurve) parameterValues

surfaceError ::
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  UvBounds ->
  Length
surfaceError fuu fuv fvv uvBounds = do
  let fuuBounds = VectorSurfaceFunction3d.evaluateBounds fuu uvBounds
  let fuvBounds = VectorSurfaceFunction3d.evaluateBounds fuv uvBounds
  let fvvBounds = VectorSurfaceFunction3d.evaluateBounds fvv uvBounds
  SurfaceLinearization.error fuuBounds fuvBounds fvvBounds uvBounds

linearizationPredicate ::
  Length ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  Curve2d Unitless UvSpace ->
  VectorCurve3d Meters space ->
  Bounds Unitless ->
  Bool
linearizationPredicate accuracy fuu fuv fvv curve2d secondDerivative3d subdomain = do
  let curveSecondDerivativeBounds = VectorCurve3d.evaluateBounds secondDerivative3d subdomain
  let curveSecondDerivativeMagnitude = VectorBounds3d.magnitude curveSecondDerivativeBounds
  let uvBounds = Curve2d.evaluateBounds curve2d subdomain
  Linearization.error curveSecondDerivativeMagnitude subdomain <= accuracy
    && surfaceError fuu fuv fvv uvBounds <= accuracy

generateSteinerPoints ::
  Length ->
  UvBounds ->
  Set2d (LineSegment2d Unitless UvSpace) Unitless UvSpace ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
  VectorSurfaceFunction3d Meters space ->
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
          & generateSteinerPoints accuracy bounds11 edgeSet fuu fuv fvv
          & generateSteinerPoints accuracy bounds12 edgeSet fuu fuv fvv
          & generateSteinerPoints accuracy bounds21 edgeSet fuu fuv fvv
          & generateSteinerPoints accuracy bounds22 edgeSet fuu fuv fvv
  case includeSubdomain uvBounds edgeSet of
    Unresolved -> recurse
    Resolved False -> accumulated
    Resolved True ->
      if surfaceError fuu fuv fvv uvBounds <= accuracy
        then UvPoint (Bounds.midpoint uBounds) (Bounds.midpoint vBounds) : accumulated
        else recurse

includeSubdomain ::
  UvBounds ->
  Set2d (LineSegment2d Unitless UvSpace) Unitless UvSpace ->
  Fuzzy Bool
includeSubdomain subdomain edgeSet = Tolerance.using Quantity.zero $
  case edgeSet of
    Set2d.Node nodeBounds leftChild rightChild
      | not (subdomain `intersects` nodeBounds) -> Resolved True
      | smallerThan nodeBounds subdomain -> do
          includeLeft <- includeSubdomain subdomain leftChild
          includeRight <- includeSubdomain subdomain rightChild
          Resolved (includeLeft && includeRight)
      | otherwise -> Unresolved
    Set2d.Leaf edgeBounds _
      | not (subdomain `intersects` edgeBounds) -> Resolved True
      | smallerThan edgeBounds subdomain -> Resolved False
      | otherwise -> Unresolved

smallerThan :: Bounds2d units space -> Bounds2d units space -> Bool
smallerThan bounds1 bounds2 = Bounds2d.diameter bounds2 <= Bounds2d.diameter bounds1
