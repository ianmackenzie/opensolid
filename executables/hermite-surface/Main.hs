module Main (main) where

import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Surface1d qualified as Expression.Surface1d
import OpenSolid.Mesh qualified as Mesh
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Stl qualified as Stl

main :: IO ()
main = IO.do
  let constant = Expression.Surface1d.constant
  let startValue = constant 1.0
  let endValue = constant 1.0 + Expression.v * constant 0.5
  let startDerivative = constant 0.0
  let endDerivative = constant 1.0 - Expression.v * constant 1.0
  let expression = Expression.hermite startValue [startDerivative] endValue [endDerivative] Expression.u
  let meshPoint uvPoint = do
        let Point2d u v = uvPoint
        Point3d.fromCoordinates Convention3d.zUp (u, v, Expression.evaluate expression uvPoint)
  let mesh = Mesh.grid 20 20 meshPoint
  Stl.writeBinary "executables/hermite-surface/mesh.stl" Convention3d.yUp identity mesh
