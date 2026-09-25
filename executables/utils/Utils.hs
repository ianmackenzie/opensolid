module Utils (showMostComplexCurve) where

import OpenSolid.Bag3D qualified as Bag3D
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
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Text qualified as Text

allEdges :: Tolerance Meters => Body3D space -> List (Surface3D.Edge space)
allEdges body =
  Bag3D.full (Body3D.surfaces body)
    & Bag3D.combine Surface3D.edges
    & Bag3D.toList

numLines :: Text -> Int
numLines text = List.length (Text.lines text)

showMostComplexCurve :: Tolerance Meters => Body3D space -> IO ()
showMostComplexCurve body = do
  let getExpression (Surface3D.Edge _ curve) = CompiledFunction.expression (Curve3D.compiled curve)
  expressions <- Result.collect getExpression (allEdges body) ?? fail
  case List.map Expression.debug expressions of
    [] -> IO.fail "No edges found"
    NonEmpty representations -> do
      let longestRepresentation = NonEmpty.maximumBy numLines representations
      IO.printLine longestRepresentation
