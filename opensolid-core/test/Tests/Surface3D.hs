module Tests.Surface3D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction3D qualified as SurfaceFunction3D
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvCurve qualified as UvCurve
import OpenSolid.UvPoint (UvPoint, data UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.VectorCurve3D (VectorCurve3D)
import OpenSolid.VectorCurve3D qualified as VectorCurve3D
import Test (Test)
import Test qualified
import Tests.Matching ((~~))
import Tests.Random qualified as Random
import Tests.SurfaceFunction3D qualified

spherePatch :: Tolerance Meters => Generator (Surface3D space)
spherePatch = do
  sketchPlane <- Random.plane3D
  let radius = Length.centimeters 10.0
  let profile =
        Curve2D.arcFrom (Point2D.x radius) (Point2D.y radius) Angle.quarterTurn
          !! error "Sphere profile should never be degenerate"
  let surface =
        Surface3D.revolved sketchPlane profile Axis2D.y Angle.quarterTurn
          !! error "Sphere patch should never be degenerate"
  Random.return surface

tests :: List Test
tests =
  [ findPoint
  , ruledSurface
  , translationalSurface
  ]

findPoint :: Test
findPoint =
  Test.group "findPoint" $
    [ findPole
    , findVertex
    , findEdgePoint
    , findInteriorPoint
    ]

findPole :: Test
findPole = Test.check 100 "findPole" do
  surface <- Test.generate spherePatch
  let point = Surface3D.pointAt (UvPoint 1.0 0.0) surface
  let solutions = Surface3D.findPoint point surface
  case solutions of
    [SurfacePoint3D.Pole (Surface3D.Pole poleCurve _)] -> do
      expectedPoleCurve <- try do UvCurve.lineFrom (UvPoint 1.0 0.0) (UvPoint 1.0 1.0)
      Test.expect (poleCurve ~~ expectedPoleCurve)
    _ -> Test.fail "Expected a single solution pole" & Test.output "solutions" solutions

findNonPole :: Text -> Generator UvPoint -> Test
findNonPole label uvPointGenerator = Test.check 100 label do
  surface <- Test.generate spherePatch
  uvPoint <- Test.generate uvPointGenerator
  let point = Surface3D.pointAt uvPoint surface
  let solutions = Surface3D.findPoint point surface
  case solutions of
    [SurfacePoint3D.Point solution _] -> Test.expect (solution ~~ uvPoint)
    _ -> Test.fail "Expected a single solution point" & Test.output "solutions" solutions

findVertex :: Test
findVertex = findNonPole "findVertex" do
  vValue <- Random.oneOf (NonEmpty.two 0.0 1.0)
  Random.return (UvPoint 0.0 vValue)

findEdgePoint :: Test
findEdgePoint = findNonPole "findEdgePoint" do
  Random.merge $
    NonEmpty.three -- Points along the three non-pole edges
      (Random.map UvPoint.left Parameter.random)
      (Random.map UvPoint.bottom Parameter.random)
      (Random.map UvPoint.top Parameter.random)

findInteriorPoint :: Test
findInteriorPoint = findNonPole "findInterior" UvPoint.random

ruledSurface :: Test
ruledSurface =
  Test.group "ruledSurface" $
    [ ruledSurfaceCorrectValue
    , ruledSurfaceDerivativeConsistency
    ]

ruledSurfaceCorrectValue :: Test
ruledSurfaceCorrectValue = Test.check 100 "ruledSurfaceCorrectValue" do
  curve1 <- Test.generate Random.cubicSpline3D
  curve2 <- Test.generate Random.cubicSpline3D
  surface <- try do Surface3D.ruled curve1 curve2
  uvPoint <- Test.generate UvPoint.random
  let UvPoint u v = uvPoint
  let p1 = Curve3D.pointAt u curve1
  let p2 = Curve3D.pointAt u curve2
  let expectedPoint = Point3D.interpolateFrom p1 p2 v
  let actualPoint = SurfaceFunction3D.pointAt uvPoint (Surface3D.function surface)
  Test.expect (actualPoint ~~ expectedPoint)
    & Test.output "expectedPoint" expectedPoint
    & Test.output "actualPoint" actualPoint

ruledSurfaceDerivativeConsistency :: Test
ruledSurfaceDerivativeConsistency = Test.check 100 "ruledSurfaceDerivativeConsistency" do
  curve1 <- Test.generate Random.cubicSpline3D
  curve2 <- Test.generate Random.cubicSpline3D
  surface <- try do Surface3D.ruled curve1 curve2
  Tests.SurfaceFunction3D.partialDerivativesAreConsistent (Surface3D.function surface)

translationalSurface :: Test
translationalSurface =
  Test.group "translationalSurface" $
    [ translationalSurfaceCorrectValue
    , translationalSurfaceDerivativeConsistency
    ]

randomVectorCubicSpline :: Generator (VectorCurve3D Meters space)
randomVectorCubicSpline =
  Random.map4
    VectorCurve3D.cubicBezier
    Random.vector3D
    Random.vector3D
    Random.vector3D
    Random.vector3D

translationalSurfaceCorrectValue :: Test
translationalSurfaceCorrectValue = Test.check 100 "translationalSurfaceCorrectValue" do
  baseCurve <- Test.generate Random.cubicSpline3D
  translationCurve <- Test.generate randomVectorCubicSpline
  surface <- try do Surface3D.sweptBy translationCurve baseCurve
  uvPoint <- Test.generate UvPoint.random
  let UvPoint u v = uvPoint
  let basePoint = Curve3D.pointAt u baseCurve
  let translationVector = VectorCurve3D.valueAt v translationCurve
  let expectedPoint = basePoint + translationVector
  let actualPoint = SurfaceFunction3D.pointAt uvPoint (Surface3D.function surface)
  Test.expect (actualPoint ~~ expectedPoint)
    & Test.output "expectedPoint" expectedPoint
    & Test.output "actualPoint" actualPoint

translationalSurfaceDerivativeConsistency :: Test
translationalSurfaceDerivativeConsistency = Test.check 100 "translationalSurfaceDerivativeConsistency" do
  baseCurve <- Test.generate Random.cubicSpline3D
  translationCurve <- Test.generate randomVectorCubicSpline
  surface <- try do Surface3D.sweptBy translationCurve baseCurve
  Tests.SurfaceFunction3D.partialDerivativesAreConsistent (Surface3D.function surface)
