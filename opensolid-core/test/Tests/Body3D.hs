module Tests.Body3D (tests) where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Area (Area)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh (Mesh)
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Number qualified as Number
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Resolution qualified as Resolution
import OpenSolid.Result qualified as Result
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Triangle3D qualified as Triangle3D
import OpenSolid.Volume (Volume)
import OpenSolid.World3D qualified as World3D
import Test

tests :: List Test
tests =
  [ sphereMeshing
  , revolvedNormals
  ]

tetrahedronVolume :: (Point3D space, Point3D space, Point3D space) -> Volume
tetrahedronVolume (p1, p2, p3) = do
  let p0 = World3D.originPoint
  (p1 - p0) `cross` (p2 - p0) `dot` (p3 - p0) / 6.0

meshArea :: Mesh (Point3D space) -> Area
meshArea mesh = Quantity.sumOf (Triangle3D.area . Triangle3D.fromVertices) (Mesh.faceVertices mesh)

meshVolume :: Mesh (Point3D space) -> Volume
meshVolume mesh = Quantity.sumOf tetrahedronVolume (Mesh.faceVertices mesh)

fractionalError :: Quantity units -> Quantity units -> Number
fractionalError actual expected = Quantity.abs (actual - expected) / expected

sphereMeshing :: Test
sphereMeshing = Test.verify "Sphere meshing" do
  let diameter = Length.centimeters 10.0
  let resolution = Resolution.maxError (0.01 * diameter)
  sphere <- Body3D.sphere (#centerPoint World3D.originPoint) (#diameter diameter) & Result.orFail
  let mesh = Body3D.toPointMesh resolution sphere
  let approximateArea = meshArea mesh
  let approximateVolume = meshVolume mesh
  let radius = 0.5 * diameter
  let exactArea = 4.0 * Number.pi * Quantity.squared radius
  let exactVolume = (4 / 3) * Number.pi * radius * radius * radius
  let areaTolerance = 0.02 * exactArea
  let volumeTolerance = 0.02 * exactVolume
  Test.all
    [ Test.expect (Tolerance.using areaTolerance (approximateArea ~= exactArea))
        & Test.output "Mesh area" approximateArea
        & Test.output "Exact area" exactArea
        & Test.output "Area error fraction" (fractionalError approximateArea exactArea)
    , Test.expect (Tolerance.using volumeTolerance (meshVolume mesh ~= exactVolume))
        & Test.output "Mesh volume" approximateVolume
        & Test.output "Exact volume" exactVolume
        & Test.output "Volume error fraction" (fractionalError approximateVolume exactVolume)
    ]

revolvedNormals :: Test
revolvedNormals = Test.verify "Revolved normals" do
  let axes = [Axis2D.y, -Axis2D.y]
  let angle = Angle.degrees 45.0
  let sweptAngles = [angle, -angle]
  let r1 = Length.centimeters 5.0
  let r2 = Length.centimeters 10.0
  let interval = Interval r1 r2
  leftRegion <- Region2D.rectangle (Bounds2D -interval interval) & Result.orFail
  rightRegion <- Region2D.rectangle (Bounds2D interval interval) & Result.orFail
  let profiles = [leftRegion, rightRegion]
  bodies <-
    Result.all
      [ Body3D.revolved World3D.frontPlane profile axis sweptAngle
      | profile <- profiles
      , axis <- axes
      , sweptAngle <- sweptAngles
      ]
      & Result.orFail
  let resolution = Resolution.maxError (0.01 * r1)
  let exactVolume =
        (r2 - r1) * 0.5 * Angle.inRadians angle * (Quantity.squared r2 - Quantity.squared r1)
  let expectCorrectVolume body = do
        let mesh = Body3D.toPointMesh resolution body
        let approximateVolume = meshVolume mesh
        let volumeTolerance = 0.02 * exactVolume
        Test.expect (Tolerance.using volumeTolerance (approximateVolume ~= exactVolume))
          & Test.output "Exact volume" exactVolume
          & Test.output "Mesh volume" approximateVolume
          & Test.output "Error fraction" (fractionalError approximateVolume exactVolume)
  Test.combine expectCorrectVolume bodies
