module Tests.Body3D (tests) where

import OpenSolid.Body3D qualified as Body3D
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Number qualified as Number
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
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
  ]

tetrahedronVolume :: (Point3D space, Point3D space, Point3D space) -> Volume
tetrahedronVolume (p1, p2, p3) = do
  let p0 = World3D.originPoint
  (p1 - p0) `cross` (p2 - p0) `dot` (p3 - p0) / 6.0

sphereMeshing :: Test
sphereMeshing = Test.verify "Sphere meshing" do
  let diameter = Length.centimeters 10.0
  let resolution = Resolution.maxError (0.01 * diameter)
  sphere <- Result.orFail (Body3D.sphere (#centerPoint World3D.originPoint) (#diameter diameter))
  let mesh = Body3D.toPointMesh resolution sphere
  let faceVertices = Mesh.faceVertices mesh
  let triangles = List.map Triangle3D.fromVertices faceVertices
  let meshArea = Quantity.sumOf Triangle3D.area triangles
  let meshVolume = Quantity.sumOf tetrahedronVolume faceVertices
  let radius = 0.5 * diameter
  let exactArea = 4.0 * Number.pi * Quantity.squared radius
  let exactVolume = (4 / 3) * Number.pi * radius * radius * radius
  let areaTolerance = 0.02 * exactArea
  let volumeTolerance = 0.02 * exactVolume
  let fractionalError actual expected = Quantity.abs (actual - expected) / expected
  Test.all
    [ Test.expect (Tolerance.using areaTolerance (meshArea ~= exactArea))
        & Test.output "Mesh area" meshArea
        & Test.output "Exact area" exactArea
        & Test.output "Area error fraction" (fractionalError meshArea exactArea)
    , Test.expect (Tolerance.using volumeTolerance (meshVolume ~= exactVolume))
        & Test.output "Mesh volume" meshVolume
        & Test.output "Exact volume" exactVolume
        & Test.output "Volume error fraction" (fractionalError meshVolume exactVolume)
    ]
