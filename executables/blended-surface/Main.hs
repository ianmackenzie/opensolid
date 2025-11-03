module Main (main) where

import OpenSolid.Array qualified as Array
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Expression qualified as Expression
import OpenSolid.IO qualified as IO
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Stl qualified as Stl
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Syntax (float, twice, (.*), (.+), (.-))
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.World3d qualified as World3d

main :: IO ()
main = Tolerance.using 1e-9 do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  f <- IO.try (SurfaceFunction.quotient (u .* (float 5.0 .- twice v)) (u .* (float 1.0 .+ v)))
  expression <- IO.try (CompiledFunction.expression f.compiled)
  IO.printLine (Expression.debug expression)
  let meshPoint uvPoint = do
        let Point2d uValue vValue = uvPoint
        Point3d.zUp uValue vValue (SurfaceFunction.evaluate f uvPoint)
  let mesh = Mesh.grid 512 512 meshPoint
  let numOriginPoints = Array.foldl (\n point -> if point == World3d.originPoint then n + 1 else n) 0 mesh.vertices
  IO.printLine ("Number of origin points in mesh: " <> Text.int numOriginPoints)
  Stl.writeBinary "executables/blended-surface/mesh.stl" Convention3d.zUp id mesh
