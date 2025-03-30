module Main (main) where

import OpenSolid.Ast qualified as Ast
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

main :: IO ()
main = IO.do
  let t = Ast.curveParameter

  IO.printLine "t^2 / (1 + t^2)"
  let tSquared = Ast.squared t
  let fraction = tSquared / (1.0 + tSquared)
  let (fractionValue, _) = Ast.compileCurve1d fraction
  IO.forEach [0 .. 5] \i -> IO.do
    let evaluated = fractionValue (Float.int i)
    IO.printLine (Text.float evaluated)

  IO.printLine "Bezier curve"
  let curve = Ast.bezierCurve1d (NonEmpty.eight 0.0 0.5 1.0 1.0 1.0 1.0 0.5 0.0) t
  let (curveValue, curveBounds) = Ast.compileCurve1d curve
  IO.forEach (Parameter.steps 10) \tValue -> IO.do
    let evaluated = curveValue tValue
    IO.printLine (Text.float evaluated)
  IO.forEach (Parameter.intervals 10) \tRange -> IO.do
    let evaluated = curveBounds tRange
    IO.printLine (Text.show evaluated)
