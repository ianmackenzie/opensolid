module Main (main) where

import OpenSolid.Array qualified as Array
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Expression qualified as Expression
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Stl qualified as Stl
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvPoint (pattern UvPoint)
import OpenSolid.World3D qualified as World3D

main :: IO ()
main = Tolerance.using 1e-9 do
  let u = SurfaceFunction1D.u
  let v = SurfaceFunction1D.v
  f <- Result.orFail (SurfaceFunction1D.quotient (u .*. (5 -. 2 *. v)) (u .*. (1 +. v)))
  expression <- Result.orFail (CompiledFunction.expression f.compiled)
  IO.printLine (Expression.debug expression)
  let meshPoint uvPoint = do
        let UvPoint uValue vValue = uvPoint
        Point3D.zUp
          (Length.meters uValue)
          (Length.meters vValue)
          (Length.meters (SurfaceFunction1D.evaluate f uvPoint))
  let mesh = Mesh.grid 512 512 meshPoint
  let numOriginPoints =
        Array.foldl
          (\n point -> if point == World3D.originPoint then n + 1 else n)
          0
          (Mesh.vertices mesh)
  IO.printLine ("Number of origin points in mesh: " <> Text.int numOriginPoints)
  Stl.writeBinary "executables/blended-surface/mesh.stl" Convention3D.zUp Length.inMeters mesh
