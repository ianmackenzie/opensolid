module Main (main) where

import OpenSolid.Array qualified as Array
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.IO qualified as IO
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Stl qualified as Stl
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance

main :: IO ()
main = Tolerance.using 1e-9 IO.do
  let u = SurfaceFunction.u
  let v = SurfaceFunction.v
  f <- SurfaceFunction.quotient (u * (5.0 - 2.0 * v)) (u * (1.0 + v))
  expression <- CompiledFunction.expression f.compiled
  IO.printLine (Expression.debug expression)
  let meshPoint uvPoint = do
        let Point2d uValue vValue = uvPoint
        Point3d.zUp uValue vValue (SurfaceFunction.evaluate f uvPoint)
  let mesh = Mesh.grid 512 512 meshPoint
  let numOriginPoints = Array.foldl (\n point -> if point == Frame3d.world.originPoint then n + 1 else n) 0 mesh.vertices
  IO.printLine ("Number of origin points in mesh: " <> Text.int numOriginPoints)
  Stl.writeBinary "executables/blended-surface/mesh.stl" Convention3d.zUp identity mesh
