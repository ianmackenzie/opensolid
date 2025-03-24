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
  let ast = tSquared / (1.0 + tSquared)
  let expr = Ast.expression1d ast
  IO.forEach [0 .. 5] \i -> IO.do
    let evaluated = Ast.evaluate expr (Float.int i)
    IO.printLine (Text.float evaluated)

  IO.printLine "Bezier curve"
  let curve = Ast.bezierCurve1d (NonEmpty.eight 0.0 0.5 1.0 1.0 1.0 1.0 0.5 0.0) t
  let curveExpr = Ast.expression1d curve
  IO.forEach (Parameter.steps 10) \tValue -> IO.do
    let evaluated = Ast.evaluate curveExpr tValue
    IO.printLine (Text.float evaluated)
