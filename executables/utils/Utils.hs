module Utils (showMostComplexCurve) where

import OpenSolid.Body3D (Body3D)
import OpenSolid.Body3D qualified as Body3D
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Expression qualified as Expression
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result
import OpenSolid.Set3D qualified as Set3D
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)
import OpenSolid.SurfaceCurve3D qualified as SurfaceCurve3D
import OpenSolid.Text qualified as Text

allSurfaceCurves :: Body3D space -> NonEmpty (SurfaceCurve3D space)
allSurfaceCurves body =
  Body3D.surfaces body
    & Set3D.combine Surface3D.boundaries
    & Set3D.flatten
    & Set3D.toNonEmpty

numLines :: Text -> Int
numLines text = List.length (Text.lines text)

showMostComplexCurve :: Body3D space -> IO ()
showMostComplexCurve body = do
  let surfaceCurves = allSurfaceCurves body
  let getExpression surfaceCurve =
        SurfaceCurve3D.curve surfaceCurve
          & Curve3D.compiled
          & CompiledFunction.expression
  expressions <- Result.collect getExpression surfaceCurves & Result.orFail
  let strings = NonEmpty.map Expression.debug expressions
  let longestString = NonEmpty.maximumBy numLines strings
  IO.printLine longestString
